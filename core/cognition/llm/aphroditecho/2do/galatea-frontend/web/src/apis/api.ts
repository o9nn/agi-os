import axios, { AxiosResponse } from 'axios';
interface SignUpResponse {
}
interface LoginResponse {
}
interface AuthStatusResponse {
}
const currentURL = `${window.location.protocol}
const tsAPI = `${currentURL}/api`;
export async function signUp(
email: string,
username: string,
password: string
): Promise<SignUpResponse> {
try {
const response: AxiosResponse<SignUpResponse> = await axios.post(
`${tsAPI}/auth/signup`,
{
email,
username,
password,
}
);
return response.data;
} catch (error) {
console.error(`Error signing up: ${error}`);
throw error;
}
}
export async function logIn(
email: string,
password: string
): Promise<LoginResponse> {
try {
const response: AxiosResponse<LoginResponse> = await axios.post(
`${tsAPI}/auth/login`,
{
email,
password,
}
);
return response.data;
} catch (error) {
console.error(`Error logging in: ${error}`);
throw error;
}
}
export async function checkAuth(): Promise<AuthStatusResponse> {
try {
const response: AxiosResponse<AuthStatusResponse> = await axios.get(
`${tsAPI}/auth/status`
);
return response.data;
} catch (error) {
console.error(`Error checking auth: ${error}`);
throw error;
}
}
export async function signOut(): Promise<void> {
try {
const response: AxiosResponse<void> = await axios.get(`${tsAPI}/auth/logout`);
return response.data;
} catch (error) {
console.error(`Error logging out: ${error}`);
throw error;
}
}