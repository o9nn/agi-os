#ifndef DSASL_CLIENT_H
#define DSASL_CLIENT_H
struct dsasl_client_settings {
const char *authid;
const char *authzid;
const char *password;
};
extern const struct dsasl_client_mech dsasl_client_mech_plain;
const struct dsasl_client_mech *dsasl_client_mech_find(const char *name);
const char *dsasl_client_mech_get_name(const struct dsasl_client_mech *mech);
struct dsasl_client *dsasl_client_new(const struct dsasl_client_mech *mech,
const struct dsasl_client_settings *set);
void dsasl_client_free(struct dsasl_client **client);
int dsasl_client_input(struct dsasl_client *client,
const unsigned char *input, size_t input_len,
const char **error_r);
int dsasl_client_output(struct dsasl_client *client,
const unsigned char **output_r, size_t *output_len_r,
const char **error_r);
int dsasl_client_set_parameter(struct dsasl_client *client,
const char *param, const char *value,
const char **error_r) ATTR_NULL(3);
int dsasl_client_get_result(struct dsasl_client *client,
const char *key, const char **value_r,
const char **error_r);
void dsasl_clients_init(void);
void dsasl_clients_deinit(void);
#endif