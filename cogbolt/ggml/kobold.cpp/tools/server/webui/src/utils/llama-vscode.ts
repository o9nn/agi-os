import { useEffect } from 'react';
import { ChatTextareaApi } from '../components/useChatTextarea.ts';
import { ChatExtraContextApi } from '../components/useChatExtraContext.tsx';
interface SetTextEvData {
  text: string;
  context: string;
}
export const useVSCodeContext = (
  textarea: ChatTextareaApi,
  extraContext: ChatExtraContextApi
) => {
  useEffect(() => {
    const handleMessage = (event: MessageEvent) => {
      if (event.data?.command === 'setText') {
        const data: SetTextEvData = event.data;
        textarea.setValue(data?.text);
        if (data?.context && data.context.length > 0) {
          extraContext.clearItems();
          extraContext.addItems([
            {
              type: 'context',
              name: 'Extra context',
              content: data.context,
            },
          ]);
        }
        textarea.focus();
        setTimeout(() => {
          textarea.refOnSubmit.current?.();
        }, 10); 
      }
    };
    window.addEventListener('message', handleMessage);
    return () => window.removeEventListener('message', handleMessage);
  }, [textarea, extraContext]);
  useEffect(() => {
    const handleKeyDown = (event: KeyboardEvent) => {
      if (event.key === 'Escape') {
        window.parent.postMessage({ command: 'escapePressed' }, '*');
      }
    };
    window.addEventListener('keydown', handleKeyDown);
    return () => window.removeEventListener('keydown', handleKeyDown);
  }, []);
  return {};
};