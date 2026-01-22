#ifndef HEADER_CAMELLIA_LOCL_H
# define HEADER_CAMELLIA_LOCL_H
typedef unsigned int u32;
typedef unsigned char u8;
int Camellia_Ekeygen(int keyBitLength, const u8 *rawKey,
KEY_TABLE_TYPE keyTable);
void Camellia_EncryptBlock_Rounds(int grandRounds, const u8 plaintext[],
const KEY_TABLE_TYPE keyTable,
u8 ciphertext[]);
void Camellia_DecryptBlock_Rounds(int grandRounds, const u8 ciphertext[],
const KEY_TABLE_TYPE keyTable,
u8 plaintext[]);
void Camellia_EncryptBlock(int keyBitLength, const u8 plaintext[],
const KEY_TABLE_TYPE keyTable, u8 ciphertext[]);
void Camellia_DecryptBlock(int keyBitLength, const u8 ciphertext[],
const KEY_TABLE_TYPE keyTable, u8 plaintext[]);
int private_Camellia_set_key(const unsigned char *userKey, const int bits,
CAMELLIA_KEY *key);
#endif