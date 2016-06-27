/* directory.cc - Results of directory-based searches from a notmuch database
 *
 * Copyright © 2009 Carl Worth
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see https://www.gnu.org/licenses/ .
 *
 * Author: Carl Worth <cworth@cworth.org>
 */

#include "notmuch-private.h"
#include "database-private.h"

/* Create an iterator to iterate over the basenames of files (or
 * directories) that all share a common parent directory.
 */
static notmuch_filenames_t *
_create_filenames_for_terms_with_prefix (void *ctx,
					 notmuch_database_t *notmuch,
					 const char *prefix)
{
    notmuch_string_list_t *filename_list;
    Xapian::TermIterator i, end;

    i = notmuch->xapian_db->allterms_begin();
    end = notmuch->xapian_db->allterms_end();
    filename_list = _notmuch_database_get_terms_with_prefix (ctx, i, end,
							     prefix);
    if (unlikely (filename_list == NULL))
	return NULL;

    return _notmuch_filenames_create (ctx, filename_list);
}

struct _notmuch_directory {
    notmuch_database_t *notmuch;
    Xapian::docid document_id;
    Xapian::Document doc;
    time_t mtime;
};

/* We end up having to call the destructor explicitly because we had
 * to use "placement new" in order to initialize C++ objects within a
 * block that we allocated with talloc. So C++ is making talloc
 * slightly less simple to use, (we wouldn't need
 * talloc_set_destructor at all otherwise).
 */
static int
_notmuch_directory_destructor (notmuch_directory_t *directory)
{
    directory->doc.~Document ();

    return 0;
}

static notmuch_private_status_t
find_directory_document (notmuch_database_t *notmuch,
			 const char *db_path,
			 Xapian::Document *document)
{
    notmuch_private_status_t status;
    Xapian::docid doc_id;

    status = _notmuch_database_find_unique_doc_id (notmuch, "directory",
						   db_path, &doc_id);
    if (status) {
	*document = Xapian::Document ();
	return status;
    }

    *document = notmuch->xapian_db->get_document (doc_id);
    return NOTMUCH_PRIVATE_STATUS_SUCCESS;
}

/* Find or create a directory document.
 *
 * 'path' should be a path relative to the path of 'database', or else
 * should be an absolute path with initial components that match the
 * path of 'database'.
 *
 * If (flags & NOTMUCH_FIND_CREATE), then the directory document will
 * be created if it does not exist.  Otherwise, if the directory
 * document does not exist, *status_ret is set to
 * NOTMUCH_STATUS_SUCCESS and this returns NULL.
 */
notmuch_directory_t *
_notmuch_directory_create (notmuch_database_t *notmuch,
			   const char *path,
			   notmuch_find_flags_t flags,
			   notmuch_status_t *status_ret)
{
    Xapian::WritableDatabase *db;
    notmuch_directory_t *directory;
    notmuch_private_status_t private_status;
    const char *db_path;
    notmuch_bool_t create = (flags & NOTMUCH_FIND_CREATE);

    if (! (notmuch->features & NOTMUCH_FEATURE_DIRECTORY_DOCS)) {
	*status_ret = NOTMUCH_STATUS_UPGRADE_REQUIRED;
	return NULL;
    }

    *status_ret = NOTMUCH_STATUS_SUCCESS;

    path = _notmuch_database_relative_path (notmuch, path);

    if (create && notmuch->mode == NOTMUCH_DATABASE_MODE_READ_ONLY)
	INTERNAL_ERROR ("Failure to ensure database is writable");

    directory = talloc (notmuch, notmuch_directory_t);
    if (unlikely (directory == NULL)) {
	*status_ret = NOTMUCH_STATUS_OUT_OF_MEMORY;
	return NULL;
    }

    directory->notmuch = notmuch;

    /* "placement new"---not actually allocating memory */
    new (&directory->doc) Xapian::Document;

    talloc_set_destructor (directory, _notmuch_directory_destructor);

    db_path = _notmuch_database_get_directory_db_path (path);

    try {
	Xapian::TermIterator i, end;

	private_status = find_directory_document (notmuch, db_path,
						  &directory->doc);
	directory->document_id = directory->doc.get_docid ();

	if (private_status == NOTMUCH_PRIVATE_STATUS_NO_DOCUMENT_FOUND) {
	    if (!create) {
		notmuch_directory_destroy (directory);
		directory = NULL;
		*status_ret = NOTMUCH_STATUS_SUCCESS;
		goto DONE;
	    }

	    void *local = talloc_new (directory);
	    const char *parent, *basename;
	    Xapian::docid parent_id;
	    char *term = talloc_asprintf (local, "%s%s",
					  _find_prefix ("directory"), db_path);
	    directory->doc.add_term (term, 0);

	    directory->doc.set_data (path);

	    _notmuch_database_split_path (local, path, &parent, &basename);

	    *status_ret = _notmuch_database_find_directory_id (
		notmuch, parent, NOTMUCH_FIND_CREATE, &parent_id);
	    if (*status_ret) {
		notmuch_directory_destroy (directory);
		directory = NULL;
		goto DONE;
	    }

	    if (basename) {
		term = talloc_asprintf (local, "%s%u:%s",
					_find_prefix ("directory-direntry"),
					parent_id, basename);
		directory->doc.add_term (term, 0);
	    }

	    directory->doc.add_value (NOTMUCH_VALUE_TIMESTAMP,
				      Xapian::sortable_serialise (0));

	    db = static_cast <Xapian::WritableDatabase *> (notmuch->xapian_db);

	    directory->document_id = _notmuch_database_generate_doc_id (notmuch);
	    db->replace_document (directory->document_id, directory->doc);
	    talloc_free (local);
	}

	directory->mtime = Xapian::sortable_unserialise (
	    directory->doc.get_value (NOTMUCH_VALUE_TIMESTAMP));
    } catch (const Xapian::Error &error) {
	_notmuch_database_log (notmuch,
		 "A Xapian exception occurred creating a directory: %s.\n",
		 error.get_msg().c_str());
	notmuch->exception_reported = TRUE;
	notmuch_directory_destroy (directory);
	directory = NULL;
	*status_ret = NOTMUCH_STATUS_XAPIAN_EXCEPTION;
    }

  DONE:
    if (db_path != path)
	free ((char *) db_path);

    return directory;
}

unsigned int
_notmuch_directory_get_document_id (notmuch_directory_t *directory)
{
    return directory->document_id;
}

notmuch_status_t
notmuch_directory_set_mtime (notmuch_directory_t *directory,
			     time_t mtime)
{
    notmuch_database_t *notmuch = directory->notmuch;
    Xapian::WritableDatabase *db;
    notmuch_status_t status;

    status = _notmuch_database_ensure_writable (notmuch);
    if (status)
	return status;

    db = static_cast <Xapian::WritableDatabase *> (notmuch->xapian_db);

    try {
	directory->doc.add_value (NOTMUCH_VALUE_TIMESTAMP,
				   Xapian::sortable_serialise (mtime));

	db->replace_document (directory->document_id, directory->doc);

	directory->mtime = mtime;

    } catch (const Xapian::Error &error) {
	_notmuch_database_log (notmuch,
		 "A Xapian exception occurred setting directory mtime: %s.\n",
		 error.get_msg().c_str());
	notmuch->exception_reported = TRUE;
	return NOTMUCH_STATUS_XAPIAN_EXCEPTION;
    }

    return NOTMUCH_STATUS_SUCCESS;
}

time_t
notmuch_directory_get_mtime (notmuch_directory_t *directory)
{
    return directory->mtime;
}

notmuch_filenames_t *
notmuch_directory_get_child_files (notmuch_directory_t *directory)
{
    char *term;
    notmuch_filenames_t *child_files;

    term = talloc_asprintf (directory, "%s%u:",
			    _find_prefix ("file-direntry"),
			    directory->document_id);

    child_files = _create_filenames_for_terms_with_prefix (directory,
							   directory->notmuch,
							   term);

    talloc_free (term);

    return child_files;
}

notmuch_filenames_t *
notmuch_directory_get_child_directories (notmuch_directory_t *directory)
{
    char *term;
    notmuch_filenames_t *child_directories;

    term = talloc_asprintf (directory, "%s%u:",
			    _find_prefix ("directory-direntry"),
			    directory->document_id);

    child_directories = _create_filenames_for_terms_with_prefix (directory,
						 directory->notmuch, term);

    talloc_free (term);

    return child_directories;
}

notmuch_status_t
notmuch_directory_delete (notmuch_directory_t *directory)
{
    notmuch_status_t status;
    Xapian::WritableDatabase *db;

    status = _notmuch_database_ensure_writable (directory->notmuch);
    if (status)
	return status;

    try {
	db = static_cast <Xapian::WritableDatabase *> (directory->notmuch->xapian_db);
	db->delete_document (directory->document_id);
    } catch (const Xapian::Error &error) {
	_notmuch_database_log (directory->notmuch,
			       "A Xapian exception occurred deleting directory entry: %s.\n",
			       error.get_msg().c_str());
	directory->notmuch->exception_reported = TRUE;
	status = NOTMUCH_STATUS_XAPIAN_EXCEPTION;
    }
    notmuch_directory_destroy (directory);

    return NOTMUCH_STATUS_SUCCESS;
}

void
notmuch_directory_destroy (notmuch_directory_t *directory)
{
    talloc_free (directory);
}
