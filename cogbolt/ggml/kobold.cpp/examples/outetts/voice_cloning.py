import outetts
print('Speaker JSON creation for Voice Cloning for OuteTTS...')
model_config = outetts.HFModelConfig_v1(model_path='OuteAI/OuteTTS-0.2-500M', language='en')
interface = outetts.InterfaceHF(model_version='0.2', cfg=model_config)
speaker = interface.create_speaker(audio_path='input_audio.wav', transcript=None, whisper_model='turbo', whisper_device=None)
interface.save_speaker(speaker, 'speaker_output.json')
print('Speaker JSON saved!')