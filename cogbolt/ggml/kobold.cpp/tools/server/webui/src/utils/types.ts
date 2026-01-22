export interface TimingReport {
  prompt_n: number;
  prompt_ms: number;
  predicted_n: number;
  predicted_ms: number;
}
export interface Message {
  id: number;
  convId: string;
  type: 'text' | 'root';
  timestamp: number; 
  role: 'user' | 'assistant' | 'system';
  content: string;
  timings?: TimingReport;
  extra?: MessageExtra[];
  parent: Message['id'];
  children: Message['id'][];
}
export type MessageExtra =
  | MessageExtraTextFile
  | MessageExtraImageFile
  | MessageExtraAudioFile
  | MessageExtraContext;
export interface MessageExtraTextFile {
  type: 'textFile';
  name: string;
  content: string;
}
export interface MessageExtraImageFile {
  type: 'imageFile';
  name: string;
  base64Url: string;
}
export interface MessageExtraAudioFile {
  type: 'audioFile';
  name: string;
  base64Data: string;
  mimeType: string;
}
export interface MessageExtraContext {
  type: 'context';
  name: string;
  content: string;
}
export type APIMessageContentPart =
  | {
      type: 'text';
      text: string;
    }
  | {
      type: 'image_url';
      image_url: { url: string };
    }
  | {
      type: 'input_audio';
      input_audio: { data: string; format: 'wav' | 'mp3' };
    };
export type APIMessage = {
  role: Message['role'];
  content: string | APIMessageContentPart[];
};
export interface Conversation {
  id: string; 
  lastModified: number; 
  currNode: Message['id']; 
  name: string;
}
export interface ViewingChat {
  conv: Readonly<Conversation>;
  messages: Readonly<Message[]>;
}
export type PendingMessage = Omit<Message, 'content'> & {
  content: string | null;
};
export enum CanvasType {
  PY_INTERPRETER,
}
export interface CanvasPyInterpreter {
  type: CanvasType.PY_INTERPRETER;
  content: string;
}
export type CanvasData = CanvasPyInterpreter;
export interface LlamaCppServerProps {
  build_info: string;
  model_path: string;
  n_ctx: number;
  modalities?: {
    vision: boolean;
    audio: boolean;
  };
}