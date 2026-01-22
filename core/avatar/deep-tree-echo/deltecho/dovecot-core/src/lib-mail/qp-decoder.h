#ifndef QP_DECODER_H
#define QP_DECODER_H
struct qp_decoder *qp_decoder_init(buffer_t *dest);
void qp_decoder_deinit(struct qp_decoder **qp);
int qp_decoder_more(struct qp_decoder *qp, const unsigned char *src,
size_t src_size, size_t *invalid_src_pos_r,
const char **error_r);
int qp_decoder_finish(struct qp_decoder *qp, const char **error_r);
#endif