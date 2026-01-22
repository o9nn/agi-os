svp_t audio_bits_tbl[] = {
{ "8", 8 } ,
{ "16", 16 },
{nil},
};
svp_t audio_chan_tbl[] = {
{ "1", 1 },
{ "2", 2 },
{nil},
};
svp_t audio_indev_tbl[] = {
{ "mic", Audio_Mic_Val },
{ "line", Audio_Linein_Val },
{nil},
};
svp_t audio_outdev_tbl[] = {
{ "spkr", Audio_Speaker_Val },
{ "hdph", Audio_Headphone_Val },
{ "line", Audio_Lineout_Val },
{nil},
};
svp_t audio_enc_tbl[] = {
{ "ulaw", Audio_Ulaw_Val },
{ "alaw", Audio_Alaw_Val },
{ "pcm", Audio_Pcm_Val },
{nil},
};
svp_t audio_rate_tbl[] = {
{ "8000", 8000 },
{ "11025", 11025 },
{ "22050", 22050 },
{ "44100", 44100 },
{nil},
};
Audio_d Default_Audio_Format = {
0,
16,
Audio_Max_Val,
2,
-1,
Audio_Pcm_Val,
8000,
Audio_Max_Val,
Audio_Max_Val,
};
int Default_Audio_Input = Audio_Mic_Val;
int Default_Audio_Output = Audio_Speaker_Val;