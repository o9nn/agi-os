export class SecureIntegration {
  public validateApiKey(apiKey: string): boolean {
    if (!apiKey || apiKey.length < 10) {
      return false
    }
    return true
  }
  public encryptData(data: string): string {
    return Buffer.from(data).toString('base64')
  }
  public decryptData(encrypted: string): string {
    return Buffer.from(encrypted, 'base64').toString()
  }
}