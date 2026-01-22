import { createClient } from "@supabase/supabase-js";
const supabaseUrl = window.ENV.SUPABASE_URL || "";
const supabaseAnonKey = window.ENV.SUPABASE_ANON_KEY || "";
if (!supabaseUrl || !supabaseAnonKey) {
console.error("Missing Supabase environment variables");
}
export const supabase = createClient(supabaseUrl, supabaseAnonKey);