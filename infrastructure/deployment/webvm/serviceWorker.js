async function handleFetch(request) {
	try {
		var r = await fetch(request);
	}
	catch (e) {
		console.error(e)
	}
	if (r.status === 0) {
		return r;
	}
	const newHeaders = new Headers(r.headers);
	newHeaders.set("Cross-Origin-Embedder-Policy", "require-corp");
	newHeaders.set("Cross-Origin-Opener-Policy", "same-origin");
	newHeaders.set("Cross-Origin-Resource-Policy", "cross-origin");
	if (r.redirected === true)
		newHeaders.set("location", r.url);
	const moddedResponse = new Response(r.redirected === true ? null : r.body, {
		headers: newHeaders,
		status: r.redirected === true ? 301 : r.status,
		statusText: r.statusText,
	});
	return moddedResponse;
}
function serviceWorkerInit() {
	self.addEventListener("install", () => self.skipWaiting());
	self.addEventListener("activate", e => e.waitUntil(self.clients.claim()));
	self.addEventListener("fetch", function (e) {
		try {
			e.respondWith(handleFetch(e.request));
		} catch (err) {
			console.log("Serviceworker NetworkError:" + err);
		}
	});
}
async function doRegister() {
	try {
		const registration = await navigator.serviceWorker.register(window.document.currentScript.src);
		console.log("Service Worker registered", registration.scope);
		registration.addEventListener("updatefound", () => {
			console.log("Reloading the page to transfer control to the Service Worker.");
			try {
				window.location.reload();
			} catch (err) {
				console.log("Service Worker failed reloading the page. ERROR:" + err);
			};
		});
		if (registration.active && !navigator.serviceWorker.controller) {
			console.log("Reloading the page to transfer control to the Service Worker.");
			try {
				window.location.reload();
			} catch (err) {
				console.log("Service Worker failed reloading the page. ERROR:" + err);
			};
		}
	}
	catch {
		console.error("Service Worker failed to register:", e)
	}
}
async function serviceWorkerRegister() {
	if (window.crossOriginIsolated) return;
	if (!window.isSecureContext) {
		console.log("Service Worker not registered, a secure context is required.");
		return;
	}
	if ("serviceWorker" in navigator)
		await doRegister();
	else
		console.log("Service worker is not supported in this browser");
}
if (typeof window === 'undefined') 
	serviceWorkerInit()
else 
	serviceWorkerRegister();