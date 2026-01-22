#ifndef QP_ENCODER_H
#define QP_ENCODER_H 1
enum qp_encoder_flag {
QP_ENCODER_FLAG_HEADER_FORMAT = 0x1,
QP_ENCODER_FLAG_BINARY_DATA = 0x2,
};
struct qp_encoder *qp_encoder_init(string_t *dest, unsigned int max_length,
enum qp_encoder_flag flags);
void qp_encoder_deinit(struct qp_encoder **qp);
void qp_encoder_more(struct qp_encoder *qp, const void *src, size_t src_size);
void qp_encoder_finish(struct qp_encoder *qp);
#endif