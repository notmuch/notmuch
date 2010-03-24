from ctypes import c_char_p
from cnotmuch.globals import nmlib, STATUS, NotmuchError

#------------------------------------------------------------------------------
class Tags(object):
    """Represents a list of notmuch tags

    This object provides an iterator over a list of notmuch tags. Do
    note that the underlying library only provides a one-time iterator
    (it cannot reset the iterator to the start). Thus iterating over
    the function will "exhaust" the list of tags, and a subsequent
    iteration attempt will raise a :exc:`NotmuchError`
    STATUS.NOT_INITIALIZED. Also note, that any function that uses
    iteration (nearly all) will also exhaust the tags. So both::

      for tag in tags: print tag 

    as well as::

       number_of_tags = len(tags)

    and even a simple::

       #str() iterates over all tags to construct a space separated list
       print(str(tags))

    will "exhaust" the Tags. If you need to re-iterate over a list of
    tags you will need to retrieve a new :class:`Tags` object.
    """

    #notmuch_tags_get
    _get = nmlib.notmuch_tags_get
    _get.restype = c_char_p

    def __init__(self, tags_p, parent=None):
        """
        :param tags_p: A pointer to an underlying *notmuch_tags_t*
             structure. These are not publically exposed, so a user
             will almost never instantiate a :class:`Tags` object
             herself. They are usually handed back as a result,
             e.g. in :meth:`Database.get_all_tags`.  *tags_p* must be
             valid, we will raise an :exc:`NotmuchError`
             (STATUS.NULL_POINTER) if it is `None`.
        :type tags_p: :class:`ctypes.c_void_p`
        :param parent: The parent object (ie :class:`Database` or 
             :class:`Message` these tags are derived from, and saves a
             reference to it, so we can automatically delete the db object
             once all derived objects are dead.
        :TODO: Make the iterator optionally work more than once by
               cache the tags in the Python object(?)
        """
        if tags_p is None:
            NotmuchError(STATUS.NULL_POINTER)

        self._tags = tags_p
        #save reference to parent object so we keep it alive
        self._parent = parent
    
    def __iter__(self):
        """ Make Tags an iterator """
        return self

    def next(self):
        if self._tags is None:
            raise NotmuchError(STATUS.NOT_INITIALIZED)

        if not nmlib.notmuch_tags_valid(self._tags):
            self._tags = None
            raise StopIteration

        tag = Tags._get (self._tags)
        nmlib.notmuch_tags_move_to_next(self._tags)
        return tag

    def __len__(self):
        """len(:class:`Tags`) returns the number of contained tags

        .. note:: As this iterates over the tags, we will not be able
               to iterate over them again (as in retrieve them)! If
               the tags have been exhausted already, this will raise a
               :exc:`NotmuchError` STATUS.NOT_INITIALIZED on
               subsequent attempts.
        """
        if self._tags is None:
            raise NotmuchError(STATUS.NOT_INITIALIZED)

        i=0
        while nmlib.notmuch_tags_valid(self._msgs):
            nmlib.notmuch_tags_move_to_next(self._msgs)
            i += 1
        self._tags = None
        return i

    def __str__(self):
        """The str() representation of Tags() is a space separated list of tags

        .. note:: As this iterates over the tags, we will not be able
               to iterate over them again (as in retrieve them)! If
               the tags have been exhausted already, this will raise a
               :exc:`NotmuchError` STATUS.NOT_INITIALIZED on
               subsequent attempts.
        """
        return " ".join(self)

    def __del__(self):
        """Close and free the notmuch tags"""
        if self._tags is not None:
            nmlib.notmuch_tags_destroy (self._tags)
