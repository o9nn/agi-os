#ifndef	XEN_RING_H
#define	XEN_RING_H
typedef uint32_t hyp_ring_pos_t;
#define hyp_ring_idx(ring, pos) (((unsigned)(pos)) & (sizeof(ring)-1))
#define hyp_ring_cell(ring, pos) (ring)[hyp_ring_idx((ring), (pos))]
#define hyp_ring_smash(ring, prod, cons) (hyp_ring_idx((ring), (prod) + 1) == \
hyp_ring_idx((ring), (cons)))
#define hyp_ring_available(ring, prod, cons) hyp_ring_idx((ring), (cons)-(prod)-1)
void hyp_ring_store(void *dest, const void *src, size_t size, void *start, void *end);
void hyp_ring_fetch(void *dest, const void *src, size_t size, void *start, void *end);
size_t hyp_ring_next_word(char **c, void *start, void *end);
#endif