import { useEffect, useRef, useState, useCallback } from 'react';
import { throttle } from '../utils/misc';
const LARGE_SCREEN_MQ = '(min-width: 1024px)';
const adjustTextareaHeight = throttle(
  (textarea: HTMLTextAreaElement | null) => {
    if (!textarea) return;
    if (!window.matchMedia(LARGE_SCREEN_MQ).matches) {
      textarea.style.height = ''; 
      textarea.style.maxHeight = '';
      return; 
    }
    const computedStyle = window.getComputedStyle(textarea);
    const currentMaxHeight = computedStyle.maxHeight;
    textarea.style.maxHeight = 'none';
    textarea.style.height = 'auto';
    textarea.style.height = `${textarea.scrollHeight}px`;
    textarea.style.maxHeight = currentMaxHeight;
  },
  100
); 
export interface ChatTextareaApi {
  value: () => string;
  setValue: (value: string) => void;
  focus: () => void;
  ref: React.RefObject<HTMLTextAreaElement>;
  refOnSubmit: React.MutableRefObject<(() => void) | null>; 
  onInput: (event: React.FormEvent<HTMLTextAreaElement>) => void; 
}
export function useChatTextarea(initValue: string): ChatTextareaApi {
  const [savedInitValue, setSavedInitValue] = useState<string>(initValue);
  const textareaRef = useRef<HTMLTextAreaElement>(null);
  const onSubmitRef = useRef<(() => void) | null>(null);
  useEffect(() => {
    const textarea = textareaRef.current;
    if (textarea) {
      if (typeof savedInitValue === 'string' && savedInitValue.length > 0) {
        textarea.value = savedInitValue;
        setTimeout(() => adjustTextareaHeight(textarea), 0);
        setSavedInitValue(''); 
      } else {
        setTimeout(() => adjustTextareaHeight(textarea), 0);
      }
    }
  }, [textareaRef, savedInitValue]); 
  const handleInput = useCallback(
    (event: React.FormEvent<HTMLTextAreaElement>) => {
      adjustTextareaHeight(event.currentTarget);
    },
    []
  );
  return {
    value: () => {
      return textareaRef.current?.value ?? '';
    },
    setValue: (value: string) => {
      const textarea = textareaRef.current;
      if (textarea) {
        textarea.value = value;
        setTimeout(() => adjustTextareaHeight(textarea), 0);
      }
    },
    focus: () => {
      if (textareaRef.current) {
        textareaRef.current.focus();
      }
    },
    ref: textareaRef,
    refOnSubmit: onSubmitRef,
    onInput: handleInput, 
  };
}