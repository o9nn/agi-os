interface AppEnv {
  SUPABASE_URL: string;
  SUPABASE_ANON_KEY: string;
}
declare global {
  interface Window {
    ENV: AppEnv;
  }
  var ENV: AppEnv;
}
export {};