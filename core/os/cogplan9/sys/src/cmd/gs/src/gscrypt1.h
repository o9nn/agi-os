#ifndef gscrypt1_INCLUDED
# define gscrypt1_INCLUDED
typedef ushort crypt_state;
int gs_type1_encrypt(byte * dest, const byte * src, uint len,
crypt_state * pstate);
int gs_type1_decrypt(byte * dest, const byte * src, uint len,
crypt_state * pstate);
#define crypt_c1 ((ushort)52845)
#define crypt_c2 ((ushort)22719)
#define crypt_c1_inverse ((ushort)27493)
#define encrypt_next(ch, state, chvar)\
(chvar = ((ch) ^ (state >> 8)),\
state = (chvar + state) * crypt_c1 + crypt_c2)
#define decrypt_this(ch, state)\
((ch) ^ (state >> 8))
#define decrypt_next(ch, state, chvar)\
(chvar = decrypt_this(ch, state),\
decrypt_skip_next(ch, state))
#define decrypt_skip_next(ch, state)\
(state = ((ch) + state) * crypt_c1 + crypt_c2)
#define decrypt_skip_previous(ch, state)\
(state = (state - crypt_c2) * crypt_c1_inverse - (ch))
#endif