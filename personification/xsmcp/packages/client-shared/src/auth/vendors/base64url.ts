export const convertBase64ToBase64url = (b64: string) => {
  return b64.endsWith('=')
    ? b64.endsWith('==')
      ? b64.replace(/\+/g, '-').replace(/\
      : b64.replace(/\+/g, '-').replace(/\
    : b64.replace(/\+/g, '-').replace(/\
}