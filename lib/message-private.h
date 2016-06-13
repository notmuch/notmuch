#ifndef MESSAGE_PRIVATE_H
#define MESSAGE_PRIVATE_H

notmuch_string_map_t *
_notmuch_message_property_map (notmuch_message_t *message);

notmuch_bool_t
_notmuch_message_frozen (notmuch_message_t *message);

void
_notmuch_message_remove_terms (notmuch_message_t *message, const char *prefix);

void
_notmuch_message_invalidate_metadata (notmuch_message_t *message,  const char *prefix_name);

#endif
