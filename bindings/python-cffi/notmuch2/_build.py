import cffi


ffibuilder = cffi.FFI()
ffibuilder.set_source(
    'notmuch2._capi',
    r"""
    #include <stdlib.h>
    #include <time.h>
    #include <notmuch.h>

    #if LIBNOTMUCH_MAJOR_VERSION < 5
        #error libnotmuch version not supported by notmuch2 python bindings
    #endif
    #if LIBNOTMUCH_MINOR_VERSION < 1
        #ERROR libnotmuch  version < 5.1 not supported
    #endif
    """,
    include_dirs=['../../lib'],
    library_dirs=['../../lib'],
    libraries=['notmuch'],
)
ffibuilder.cdef(
    r"""
    void free(void *ptr);
    typedef int... time_t;

    #define LIBNOTMUCH_MAJOR_VERSION ...
    #define LIBNOTMUCH_MINOR_VERSION ...
    #define LIBNOTMUCH_MICRO_VERSION ...

    #define NOTMUCH_TAG_MAX ...

    typedef enum _notmuch_status {
        NOTMUCH_STATUS_SUCCESS = 0,
        NOTMUCH_STATUS_OUT_OF_MEMORY,
        NOTMUCH_STATUS_READ_ONLY_DATABASE,
        NOTMUCH_STATUS_XAPIAN_EXCEPTION,
        NOTMUCH_STATUS_FILE_ERROR,
        NOTMUCH_STATUS_FILE_NOT_EMAIL,
        NOTMUCH_STATUS_DUPLICATE_MESSAGE_ID,
        NOTMUCH_STATUS_NULL_POINTER,
        NOTMUCH_STATUS_TAG_TOO_LONG,
        NOTMUCH_STATUS_UNBALANCED_FREEZE_THAW,
        NOTMUCH_STATUS_UNBALANCED_ATOMIC,
        NOTMUCH_STATUS_UNSUPPORTED_OPERATION,
        NOTMUCH_STATUS_UPGRADE_REQUIRED,
        NOTMUCH_STATUS_PATH_ERROR,
        NOTMUCH_STATUS_ILLEGAL_ARGUMENT,
        NOTMUCH_STATUS_MALFORMED_CRYPTO_PROTOCOL,
        NOTMUCH_STATUS_FAILED_CRYPTO_CONTEXT_CREATION,
        NOTMUCH_STATUS_UNKNOWN_CRYPTO_PROTOCOL,
        NOTMUCH_STATUS_NO_CONFIG,
        NOTMUCH_STATUS_NO_DATABASE,
        NOTMUCH_STATUS_DATABASE_EXISTS,
        NOTMUCH_STATUS_BAD_QUERY_SYNTAX,
        NOTMUCH_STATUS_LAST_STATUS
    } notmuch_status_t;
    typedef enum {
        NOTMUCH_DATABASE_MODE_READ_ONLY = 0,
        NOTMUCH_DATABASE_MODE_READ_WRITE
    } notmuch_database_mode_t;
    typedef int notmuch_bool_t;
    typedef enum _notmuch_message_flag {
        NOTMUCH_MESSAGE_FLAG_MATCH,
        NOTMUCH_MESSAGE_FLAG_EXCLUDED,
        NOTMUCH_MESSAGE_FLAG_GHOST,
    } notmuch_message_flag_t;
    typedef enum {
        NOTMUCH_SORT_OLDEST_FIRST,
        NOTMUCH_SORT_NEWEST_FIRST,
        NOTMUCH_SORT_MESSAGE_ID,
        NOTMUCH_SORT_UNSORTED
    } notmuch_sort_t;
    typedef enum {
        NOTMUCH_EXCLUDE_FLAG,
        NOTMUCH_EXCLUDE_TRUE,
        NOTMUCH_EXCLUDE_FALSE,
        NOTMUCH_EXCLUDE_ALL
    } notmuch_exclude_t;
    typedef enum {
        NOTMUCH_DECRYPT_FALSE,
        NOTMUCH_DECRYPT_TRUE,
        NOTMUCH_DECRYPT_AUTO,
        NOTMUCH_DECRYPT_NOSTASH,
    } notmuch_decryption_policy_t;

    // These are fully opaque types for us, we only ever use pointers.
    typedef struct _notmuch_database notmuch_database_t;
    typedef struct _notmuch_query notmuch_query_t;
    typedef struct _notmuch_threads notmuch_threads_t;
    typedef struct _notmuch_thread notmuch_thread_t;
    typedef struct _notmuch_messages notmuch_messages_t;
    typedef struct _notmuch_message notmuch_message_t;
    typedef struct _notmuch_tags notmuch_tags_t;
    typedef struct _notmuch_string_map_iterator notmuch_message_properties_t;
    typedef struct _notmuch_directory notmuch_directory_t;
    typedef struct _notmuch_filenames notmuch_filenames_t;
    typedef struct _notmuch_config_list notmuch_config_list_t;
    typedef struct _notmuch_indexopts notmuch_indexopts_t;

    const char *
    notmuch_status_to_string (notmuch_status_t status);

    notmuch_status_t
    notmuch_database_create_verbose (const char *path,
                                     notmuch_database_t **database,
                                     char **error_message);
    notmuch_status_t
    notmuch_database_create (const char *path, notmuch_database_t **database);
    notmuch_status_t
    notmuch_database_open_verbose (const char *path,
                                   notmuch_database_mode_t mode,
                                   notmuch_database_t **database,
                                   char **error_message);
    notmuch_status_t
    notmuch_database_open (const char *path,
                           notmuch_database_mode_t mode,
                           notmuch_database_t **database);
    notmuch_status_t
    notmuch_database_close (notmuch_database_t *database);
    notmuch_status_t
    notmuch_database_destroy (notmuch_database_t *database);
    const char *
    notmuch_database_get_path (notmuch_database_t *database);
    unsigned int
    notmuch_database_get_version (notmuch_database_t *database);
    notmuch_bool_t
    notmuch_database_needs_upgrade (notmuch_database_t *database);
    notmuch_status_t
    notmuch_database_begin_atomic (notmuch_database_t *notmuch);
    notmuch_status_t
    notmuch_database_end_atomic (notmuch_database_t *notmuch);
    unsigned long
    notmuch_database_get_revision (notmuch_database_t *notmuch,
                                   const char **uuid);
    notmuch_status_t
    notmuch_database_index_file (notmuch_database_t *database,
                                 const char *filename,
                                 notmuch_indexopts_t *indexopts,
                                 notmuch_message_t **message);
    notmuch_status_t
    notmuch_database_remove_message (notmuch_database_t *database,
                                     const char *filename);
    notmuch_status_t
    notmuch_database_find_message (notmuch_database_t *database,
                                   const char *message_id,
                                   notmuch_message_t **message);
    notmuch_status_t
    notmuch_database_find_message_by_filename (notmuch_database_t *notmuch,
                                               const char *filename,
                                               notmuch_message_t **message);
    notmuch_tags_t *
    notmuch_database_get_all_tags (notmuch_database_t *db);

    notmuch_query_t *
    notmuch_query_create (notmuch_database_t *database,
                          const char *query_string);
    const char *
    notmuch_query_get_query_string (const notmuch_query_t *query);
    notmuch_database_t *
    notmuch_query_get_database (const notmuch_query_t *query);
    void
    notmuch_query_set_omit_excluded (notmuch_query_t *query,
                                     notmuch_exclude_t omit_excluded);
    void
    notmuch_query_set_sort (notmuch_query_t *query, notmuch_sort_t sort);
    notmuch_sort_t
    notmuch_query_get_sort (const notmuch_query_t *query);
    notmuch_status_t
    notmuch_query_add_tag_exclude (notmuch_query_t *query, const char *tag);
    notmuch_status_t
    notmuch_query_search_threads (notmuch_query_t *query,
                                  notmuch_threads_t **out);
    notmuch_status_t
    notmuch_query_search_messages (notmuch_query_t *query,
                                   notmuch_messages_t **out);
    notmuch_status_t
    notmuch_query_count_messages (notmuch_query_t *query, unsigned int *count);
    notmuch_status_t
    notmuch_query_count_threads (notmuch_query_t *query, unsigned *count);
    void
    notmuch_query_destroy (notmuch_query_t *query);

    notmuch_bool_t
    notmuch_threads_valid (notmuch_threads_t *threads);
    notmuch_thread_t *
    notmuch_threads_get (notmuch_threads_t *threads);
    void
    notmuch_threads_move_to_next (notmuch_threads_t *threads);
    void
    notmuch_threads_destroy (notmuch_threads_t *threads);

    const char *
    notmuch_thread_get_thread_id (notmuch_thread_t *thread);
    notmuch_messages_t *
    notmuch_message_get_replies (notmuch_message_t *message);
    int
    notmuch_thread_get_total_messages (notmuch_thread_t *thread);
    notmuch_messages_t *
    notmuch_thread_get_toplevel_messages (notmuch_thread_t *thread);
    notmuch_messages_t *
    notmuch_thread_get_messages (notmuch_thread_t *thread);
    int
    notmuch_thread_get_matched_messages (notmuch_thread_t *thread);
    const char *
    notmuch_thread_get_authors (notmuch_thread_t *thread);
    const char *
    notmuch_thread_get_subject (notmuch_thread_t *thread);
    time_t
    notmuch_thread_get_oldest_date (notmuch_thread_t *thread);
    time_t
    notmuch_thread_get_newest_date (notmuch_thread_t *thread);
    notmuch_tags_t *
    notmuch_thread_get_tags (notmuch_thread_t *thread);
    void
    notmuch_thread_destroy (notmuch_thread_t *thread);

    notmuch_bool_t
    notmuch_messages_valid (notmuch_messages_t *messages);
    notmuch_message_t *
    notmuch_messages_get (notmuch_messages_t *messages);
    void
    notmuch_messages_move_to_next (notmuch_messages_t *messages);
    void
    notmuch_messages_destroy (notmuch_messages_t *messages);
    notmuch_tags_t *
    notmuch_messages_collect_tags (notmuch_messages_t *messages);

    const char *
    notmuch_message_get_message_id (notmuch_message_t *message);
    const char *
    notmuch_message_get_thread_id (notmuch_message_t *message);
    const char *
    notmuch_message_get_filename (notmuch_message_t *message);
    notmuch_filenames_t *
    notmuch_message_get_filenames (notmuch_message_t *message);
    notmuch_bool_t
    notmuch_message_get_flag (notmuch_message_t *message,
                              notmuch_message_flag_t flag);
    void
    notmuch_message_set_flag (notmuch_message_t *message,
                              notmuch_message_flag_t flag,
                              notmuch_bool_t value);
    time_t
    notmuch_message_get_date  (notmuch_message_t *message);
    const char *
    notmuch_message_get_header (notmuch_message_t *message,
                                const char *header);
    notmuch_tags_t *
    notmuch_message_get_tags (notmuch_message_t *message);
    notmuch_status_t
    notmuch_message_add_tag (notmuch_message_t *message, const char *tag);
    notmuch_status_t
    notmuch_message_remove_tag (notmuch_message_t *message, const char *tag);
    notmuch_status_t
    notmuch_message_remove_all_tags (notmuch_message_t *message);
    notmuch_status_t
    notmuch_message_maildir_flags_to_tags (notmuch_message_t *message);
    notmuch_status_t
    notmuch_message_tags_to_maildir_flags (notmuch_message_t *message);
    notmuch_status_t
    notmuch_message_freeze (notmuch_message_t *message);
    notmuch_status_t
    notmuch_message_thaw (notmuch_message_t *message);
    notmuch_status_t
    notmuch_message_get_property (notmuch_message_t *message,
                                  const char *key, const char **value);
    notmuch_status_t
    notmuch_message_add_property (notmuch_message_t *message,
                                  const char *key, const char *value);
    notmuch_status_t
    notmuch_message_remove_property (notmuch_message_t *message,
                                     const char *key, const char *value);
    notmuch_status_t
    notmuch_message_remove_all_properties (notmuch_message_t *message,
                                           const char *key);
    notmuch_message_properties_t *
    notmuch_message_get_properties (notmuch_message_t *message,
                                    const char *key, notmuch_bool_t exact);
    notmuch_bool_t
    notmuch_message_properties_valid (notmuch_message_properties_t
                                          *properties);
    void
    notmuch_message_properties_move_to_next (notmuch_message_properties_t
                                                 *properties);
    const char *
    notmuch_message_properties_key (notmuch_message_properties_t *properties);
    const char *
    notmuch_message_properties_value (notmuch_message_properties_t
                                          *properties);
    void
    notmuch_message_properties_destroy (notmuch_message_properties_t
                                            *properties);
    void
    notmuch_message_destroy (notmuch_message_t *message);

    notmuch_bool_t
    notmuch_tags_valid (notmuch_tags_t *tags);
    const char *
    notmuch_tags_get (notmuch_tags_t *tags);
    void
    notmuch_tags_move_to_next (notmuch_tags_t *tags);
    void
    notmuch_tags_destroy (notmuch_tags_t *tags);

    notmuch_bool_t
    notmuch_filenames_valid (notmuch_filenames_t *filenames);
    const char *
    notmuch_filenames_get (notmuch_filenames_t *filenames);
    void
    notmuch_filenames_move_to_next (notmuch_filenames_t *filenames);
    void
    notmuch_filenames_destroy (notmuch_filenames_t *filenames);
    notmuch_indexopts_t *
    notmuch_database_get_default_indexopts (notmuch_database_t *db);
    notmuch_status_t
    notmuch_indexopts_set_decrypt_policy (notmuch_indexopts_t *indexopts,
                                          notmuch_decryption_policy_t decrypt_policy);
    notmuch_decryption_policy_t
    notmuch_indexopts_get_decrypt_policy (const notmuch_indexopts_t *indexopts);
    void
    notmuch_indexopts_destroy (notmuch_indexopts_t *options);

    notmuch_status_t
    notmuch_database_set_config (notmuch_database_t *db, const char *key, const char *value);
    notmuch_status_t
    notmuch_database_get_config (notmuch_database_t *db, const char *key, char **value);
    notmuch_status_t
    notmuch_database_get_config_list (notmuch_database_t *db, const char *prefix, notmuch_config_list_t **out);
    notmuch_bool_t
    notmuch_config_list_valid (notmuch_config_list_t *config_list);
    const char *
    notmuch_config_list_key (notmuch_config_list_t *config_list);
    const char *
    notmuch_config_list_value (notmuch_config_list_t *config_list);
    void
    notmuch_config_list_move_to_next (notmuch_config_list_t *config_list);
    void
    notmuch_config_list_destroy (notmuch_config_list_t *config_list);
    """
)


if __name__ == '__main__':
    ffibuilder.compile(verbose=True)
