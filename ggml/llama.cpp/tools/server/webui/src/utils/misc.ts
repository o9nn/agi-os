import TextLineStream from 'textlinestream';
import {
  APIMessage,
  APIMessageContentPart,
  LlamaCppServerProps,
  Message,
} from './types';
import { asyncIterator } from '@sec-ant/readable-stream/ponyfill/asyncIterator';
export const isString = (x: any) => !!x.toLowerCase;
export const isBoolean = (x: any) => x === true || x === false;
export const isNumeric = (n: any) => !isString(n) && !isNaN(n) && !isBoolean(n);
export const escapeAttr = (str: string) =>
  str.replace(/>/g, '&gt;').replace(/"/g, '&quot;');
export async function* getSSEStreamAsync(fetchResponse: Response) {
  if (!fetchResponse.body) throw new Error('Response body is empty');
  const lines: ReadableStream<string> = fetchResponse.body
    .pipeThrough(new TextDecoderStream())
    .pipeThrough(new TextLineStream());
  for await (const line of asyncIterator(lines)) {
    if (line.startsWith('data:') && !line.endsWith('[DONE]')) {
      const data = JSON.parse(line.slice(5));
      yield data;
    } else if (line.startsWith('error:')) {
      const data = JSON.parse(line.slice(6));
      throw new Error(data.message || 'Unknown error');
    }
  }
}
export const copyStr = (textToCopy: string) => {
  if (navigator.clipboard && window.isSecureContext) {
    navigator.clipboard.writeText(textToCopy);
  } else {
    const textArea = document.createElement('textarea');
    textArea.value = textToCopy;
    textArea.style.position = 'absolute';
    textArea.style.left = '-999999px';
    document.body.prepend(textArea);
    textArea.select();
    document.execCommand('copy');
  }
};
export function normalizeMsgsForAPI(messages: Readonly<Message[]>) {
  return messages.map((msg) => {
    if (msg.role !== 'user' || !msg.extra) {
      return {
        role: msg.role,
        content: msg.content,
      } as APIMessage;
    }
    const contentArr: APIMessageContentPart[] = [];
    for (const extra of msg.extra ?? []) {
      if (extra.type === 'context') {
        contentArr.push({
          type: 'text',
          text: extra.content,
        });
      } else if (extra.type === 'textFile') {
        contentArr.push({
          type: 'text',
          text: `File: ${extra.name}\nContent:\n\n${extra.content}`,
        });
      } else if (extra.type === 'imageFile') {
        contentArr.push({
          type: 'image_url',
          image_url: { url: extra.base64Url },
        });
      } else if (extra.type === 'audioFile') {
        contentArr.push({
          type: 'input_audio',
          input_audio: {
            data: extra.base64Data,
            format: /wav/.test(extra.mimeType) ? 'wav' : 'mp3',
          },
        });
      } else {
        throw new Error('Unknown extra type');
      }
    }
    contentArr.push({
      type: 'text',
      text: msg.content,
    });
    return {
      role: msg.role,
      content: contentArr,
    };
  }) as APIMessage[];
}
export function filterThoughtFromMsgs(messages: APIMessage[]) {
  console.debug({ messages });
  return messages.map((msg) => {
    if (msg.role !== 'assistant') {
      return msg;
    }
    const contentStr = msg.content as string;
    return {
      role: msg.role,
      content:
        msg.role === 'assistant'
          ? contentStr.split('</think>').at(-1)!.trim()
          : contentStr,
    } as APIMessage;
  });
}
export function classNames(classes: Record<string, boolean>): string {
  return Object.entries(classes)
    .filter(([_, value]) => value)
    .map(([key, _]) => key)
    .join(' ');
}
export const delay = (ms: number) =>
  new Promise((resolve) => setTimeout(resolve, ms));
export const throttle = <T extends unknown[]>(
  callback: (...args: T) => void,
  delay: number
) => {
  let isWaiting = false;
  return (...args: T) => {
    if (isWaiting) {
      return;
    }
    callback(...args);
    isWaiting = true;
    setTimeout(() => {
      isWaiting = false;
    }, delay);
  };
};
export const cleanCurrentUrl = (removeQueryParams: string[]) => {
  const url = new URL(window.location.href);
  removeQueryParams.forEach((param) => {
    url.searchParams.delete(param);
  });
  window.history.replaceState({}, '', url.toString());
};
export const getServerProps = async (
  baseUrl: string,
  apiKey?: string
): Promise<LlamaCppServerProps> => {
  try {
    const response = await fetch(`${baseUrl}/props`, {
      headers: {
        'Content-Type': 'application/json',
        ...(apiKey ? { Authorization: `Bearer ${apiKey}` } : {}),
      },
    });
    if (!response.ok) {
      throw new Error('Failed to fetch server props');
    }
    const data = await response.json();
    return data as LlamaCppServerProps;
  } catch (error) {
    console.error('Error fetching server props:', error);
    throw error;
  }
};