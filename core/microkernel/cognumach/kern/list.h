#ifndef _KERN_LIST_H
#define _KERN_LIST_H
#include <stddef.h>
#include <sys/types.h>
#include <kern/macros.h>
struct list {
struct list *prev;
struct list *next;
};
#define LIST_INITIALIZER(list) { &(list), &(list) }
static inline void list_init(struct list *list)
{
list->prev = list;
list->next = list;
}
static inline void list_node_init(struct list *node)
{
node->prev = NULL;
node->next = NULL;
}
static inline int list_node_unlinked(const struct list *node)
{
return node->prev == NULL;
}
#define list_entry(node, type, member) structof(node, type, member)
static inline struct list * list_first(const struct list *list)
{
return list->next;
}
static inline struct list * list_last(const struct list *list)
{
return list->prev;
}
static inline struct list * list_next(const struct list *node)
{
return node->next;
}
static inline struct list * list_prev(const struct list *node)
{
return node->prev;
}
#define list_first_entry(list, type, member) \
list_entry(list_first(list), type, member)
#define list_last_entry(list, type, member) \
list_entry(list_last(list), type, member)
static inline int list_end(const struct list *list, const struct list *node)
{
return list == node;
}
static inline int list_empty(const struct list *list)
{
return list == list->next;
}
static inline int list_singular(const struct list *list)
{
return (list != list->next) && (list->next == list->prev);
}
static inline void list_split(struct list *list1, struct list *list2,
struct list *node)
{
if (list_empty(list2) || (list2->next == node) || list_end(list2, node))
return;
list1->next = list2->next;
list1->next->prev = list1;
list1->prev = node->prev;
node->prev->next = list1;
list2->next = node;
node->prev = list2;
}
static inline void list_concat(struct list *list1, const struct list *list2)
{
struct list *last1, *first2, *last2;
if (list_empty(list2))
return;
last1 = list1->prev;
first2 = list2->next;
last2 = list2->prev;
last1->next = first2;
first2->prev = last1;
last2->next = list1;
list1->prev = last2;
}
static inline void list_set_head(struct list *new_head,
const struct list *old_head)
{
if (list_empty(old_head)) {
list_init(new_head);
return;
}
*new_head = *old_head;
new_head->next->prev = new_head;
new_head->prev->next = new_head;
}
static inline void list_add(struct list *prev, struct list *next,
struct list *node)
{
next->prev = node;
node->next = next;
prev->next = node;
node->prev = prev;
}
static inline void list_insert_head(struct list *list, struct list *node)
{
list_add(list, list->next, node);
}
static inline void list_insert_tail(struct list *list, struct list *node)
{
list_add(list->prev, list, node);
}
static inline void list_insert_before(struct list *next, struct list *node)
{
list_add(next->prev, next, node);
}
static inline void list_insert_after(struct list *prev, struct list *node)
{
list_add(prev, prev->next, node);
}
static inline void list_remove(struct list *node)
{
node->prev->next = node->next;
node->next->prev = node->prev;
}
#define list_for_each(list, node)   \
for (node = list_first(list);       \
!list_end(list, node);         \
node = list_next(node))
#define list_for_each_safe(list, node, tmp)             \
for (node = list_first(list), tmp = list_next(node);    \
!list_end(list, node);                             \
node = tmp, tmp = list_next(node))
#define list_for_each_reverse(list, node)   \
for (node = list_last(list);                \
!list_end(list, node);                 \
node = list_prev(node))
#define list_for_each_reverse_safe(list, node, tmp) \
for (node = list_last(list), tmp = list_prev(node); \
!list_end(list, node);                         \
node = tmp, tmp = list_prev(node))
#define list_for_each_entry(list, entry, member)                    \
for (entry = list_entry(list_first(list), typeof(*entry), member);  \
!list_end(list, &entry->member);                               \
entry = list_entry(list_next(&entry->member), typeof(*entry),  \
member))
#define list_for_each_entry_safe(list, entry, tmp, member)          \
for (entry = list_entry(list_first(list), typeof(*entry), member),  \
tmp = list_entry(list_next(&entry->member), typeof(*entry),  \
member);                                    \
!list_end(list, &entry->member);                               \
entry = tmp, tmp = list_entry(list_next(&entry->member),       \
typeof(*entry), member))
#define list_for_each_entry_reverse(list, entry, member)            \
for (entry = list_entry(list_last(list), typeof(*entry), member);   \
!list_end(list, &entry->member);                               \
entry = list_entry(list_prev(&entry->member), typeof(*entry),  \
member))
#define list_for_each_entry_reverse_safe(list, entry, tmp, member)  \
for (entry = list_entry(list_last(list), typeof(*entry), member),   \
tmp = list_entry(list_prev(&entry->member), typeof(*entry),  \
member);                                    \
!list_end(list, &entry->member);                               \
entry = tmp, tmp = list_entry(list_prev(&entry->member),       \
typeof(*entry), member))
#endif