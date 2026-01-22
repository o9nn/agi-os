import os
import time
import logging
import cv2
import random
from playwright.sync_api import sync_playwright
import numpy as np
from urllib.parse import urlparse
from dotenv import load_dotenv
from deep_tree_echo import DeepTreeEcho, TreeNode
from datetime import datetime
import json
import contextlib
try:
    from chat_session_manager import session_manager, ChatPlatform, log_chat_message, create_chat_session
    SESSION_MANAGER_AVAILABLE = True
except ImportError:
    SESSION_MANAGER_AVAILABLE = False
    logging.warning("Session manager not available - conversations won't be auto-saved")
os.makedirs('templates', exist_ok=True)
load_dotenv()
class SeleniumInterface:
    def __init__(self):
        self.browser = None
        self.page = None
        self.context = None
        self.playwright = None
        self.echo_system = DeepTreeEcho(echo_threshold=0.75)
        self.conversation_history = []
        self.session_start_time = datetime.now()
        self.last_action_time = time.time()
        self.memory_file = os.path.join('activity_logs', 'browser', 'chat_memory.json')
        self.chat_session_id = None
        self.auto_save_enabled = SESSION_MANAGER_AVAILABLE
        os.makedirs(os.path.join('activity_logs', 'browser'), exist_ok=True)
        logging.basicConfig(level=logging.DEBUG)
        self.logger = logging.getLogger(__name__)
        if SESSION_MANAGER_AVAILABLE:
            try:
                self.chat_session_id = create_chat_session('browser', f"Browser Session - {datetime.now().strftime('%Y-%m-%d %H:%M')}")
                self.logger.info(f'Created browser chat session: {self.chat_session_id}')
            except Exception as e:
                self.logger.warning(f'Failed to initialize session manager: {str(e)}')
                self.auto_save_enabled = False
        self.typing_speed_wpm = random.randint(30, 80)
        self.human_delay_min = 0.1
        self.human_delay_max = 1.0
    def find_existing_browser(self):
        try:
            ports = [9222, 9223, 9224, 9225]
            for port in ports:
                try:
                    browser = self.playwright.chromium.connect_over_cdp(f'http://localhost:{port}')
                    for context in browser.contexts:
                        for page in context.pages:
                            parsed_url = urlparse(page.url)
                            if parsed_url.hostname and (parsed_url.hostname == 'chatgpt.com' or parsed_url.hostname.endswith('.chatgpt.com')):
                                self.browser = browser
                                self.page = page
                                self.logger.info(f'Connected to existing ChatGPT session on port {port}')
                                return True
                except Exception:
                    continue
        except Exception as e:
            self.logger.debug(f'Error finding existing browser: {str(e)}')
        return False
    def init(self):
        try:
            self.playwright = sync_playwright().start()
            if self.find_existing_browser():
                return True
            self.logger.info('No existing browser session found, creating new one...')
            user_data_dir = os.path.join(os.getcwd(), 'chrome_user_data')
            os.makedirs(user_data_dir, exist_ok=True)
            self.browser = self.playwright.chromium.launch_persistent_context(user_data_dir=user_data_dir, headless=False, args=['--no-sandbox', '--disable-setuid-sandbox', '--window-size=1920,1080', '--start-maximized', '--disable-dev-shm-usage', '--disable-blink-features=AutomationControlled', '--remote-debugging-port=9222', '--disable-features=IsolateOrigins,site-per-process', '--user-agent=Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/123.0.0.0 Safari/537.36'], ignore_https_errors=True, viewport={'width': 1920, 'height': 1080})
            self.page = self.browser.pages[0] if self.browser.pages else self.browser.new_page()
            self.page.set_default_timeout(60000)
            self.page.add_init_script("\n                // Overwrite the 'chrome' property to avoid detection\n                Object.defineProperty(window, 'chrome', {\n                    value: new Proxy({}, {\n                        get: function(target, name) {\n                            if (name === 'runtime') return {};\n                            return function() {};\n                        }\n                    })\n                });\n                \n                // Modify navigator properties\n                const originalNavigator = window.navigator;\n                const navigatorProxy = new Proxy(originalNavigator, {\n                    get: function(target, name) {\n                        switch (name) {\n                            case 'webdriver':\n                                return undefined;\n                            case 'languages':\n                                return ['en-US', 'en'];\n                            case 'plugins':\n                                return [\n                                    {description: 'PDF Viewer', filename: 'internal-pdf-viewer'},\n                                    {description: 'Chrome PDF Viewer', filename: 'chrome-pdf-viewer'},\n                                    {description: 'Chromium PDF Viewer', filename: 'chromium-pdf-viewer'},\n                                    {description: 'Microsoft Edge PDF Viewer', filename: 'edge-pdf-viewer'},\n                                    {description: 'WebKit built-in PDF', filename: 'webkit-pdf-viewer'}\n                                ];\n                            default:\n                                return typeof target[name] === 'function' ? target[name].bind(target) : target[name];\n                        }\n                    }\n                });\n                Object.defineProperty(window, 'navigator', {\n                    value: navigatorProxy\n                });\n                \n                // Overwrite permissions\n                const originalPermissions = window.Permissions;\n                window.Permissions = {\n                    query: async () => { return { state: 'granted', onchange: null }; }\n                };\n                \n                // Add fake canvas fingerprinting\n                const originalToDataURL = HTMLCanvasElement.prototype.toDataURL;\n                HTMLCanvasElement.prototype.toDataURL = function(type) {\n                    if (window.canvas_fingerprint_warned) return originalToDataURL.apply(this, arguments);\n                    const canvas = this;\n                    window.canvas_fingerprint_warned = true;\n                    return originalToDataURL.apply(canvas, arguments);\n                };\n            ")
            self._setup_event_listeners()
            if 'chat.openai.com' not in self.page.url and 'chatgpt.com' not in self.page.url:
                self.logger.info('Navigating to chat page...')
                max_retries = 3
                for attempt in range(max_retries):
                    try:
                        self.page.goto('https://chat.openai.com', wait_until='networkidle')
                        self.logger.info(f'Current URL: {self.page.url}')
                        if not self.wait_for_cloudflare():
                            if attempt < max_retries - 1:
                                self.logger.warning(f'Cloudflare challenge failed, attempt {attempt + 1}/{max_retries}')
                                time.sleep(5)
                                continue
                            else:
                                self.logger.error('Failed to pass Cloudflare challenge after all retries')
                                return False
                        self._wait_for_page_stability()
                        break
                    except Exception as e:
                        self.logger.error(f'Navigation attempt {attempt + 1} failed: {str(e)}')
                        if attempt < max_retries - 1:
                            self._simulate_human_delay()
                        else:
                            raise
            self._load_memory()
            return True
        except Exception as e:
            self.logger.error(f'Failed to initialize browser: {str(e)}')
            if self.page:
                self.page.screenshot(path='init_error.png')
            return False
    def _setup_event_listeners(self):
        self.page.on('console', lambda msg: self._handle_console_message(msg))
        self.page.on('pageerror', lambda err: self.logger.error(f'Page error: {err}'))
        self.page.on('dialog', lambda dialog: self._handle_dialog(dialog))
        self.page.on('framenavigated', lambda frame: self._handle_navigation(frame))
    def _handle_console_message(self, msg):
        if msg.type == 'error':
            self.logger.warning(f'Console error: {msg.text}')
        elif 'cloudflare' in msg.text.lower():
            self.logger.info(f'Cloudflare related message: {msg.text}')
    def _handle_dialog(self, dialog):
        self.logger.info(f'Dialog: {dialog.type} - {dialog.message}')
        if dialog.type == 'confirm' or dialog.type == 'beforeunload':
            dialog.accept()
        else:
            dialog.dismiss()
    def _handle_navigation(self, frame):
        if frame == self.page.main_frame:
            self.logger.info(f'Navigated to: {frame.url}')
            if 'cloudflare' in frame.url.lower() or 'challenge' in frame.url.lower():
                self.logger.info('Detected Cloudflare navigation, waiting...')
    def find_element_by_image(self, template_path, threshold=0.8):
        try:
            screenshot_bytes = self.page.screenshot(type='png')
            nparr = np.frombuffer(screenshot_bytes, np.uint8)
            screenshot = cv2.imdecode(nparr, cv2.IMREAD_COLOR)
            template = cv2.imread(template_path)
            if template is None:
                self.logger.error(f'Could not load template image: {template_path}')
                return None
            result = cv2.matchTemplate(screenshot, template, cv2.TM_CCOEFF_NORMED)
            min_val, max_val, min_loc, max_loc = cv2.minMaxLoc(result)
            if max_val >= threshold:
                h, w = template.shape[:2]
                center_x = max_loc[0] + w // 2
                center_y = max_loc[1] + h // 2
                self.logger.info(f'Found match for {template_path} at ({center_x}, {center_y}) with confidence {max_val}')
                return (center_x, center_y)
            else:
                self.logger.debug(f'No match found for {template_path} (best match: {max_val})')
                return None
        except Exception as e:
            self.logger.error(f'Error in visual search: {str(e)}')
            return None
    def _simulate_human_delay(self, min_delay=None, max_delay=None):
        if min_delay is None:
            min_delay = self.human_delay_min
        if max_delay is None:
            max_delay = self.human_delay_max
        delay = random.uniform(min_delay, max_delay)
        time.sleep(delay)
        return delay
    def _calculate_typing_delay(self, text):
        char_count = len(text)
        word_count = char_count / 5
        minutes = word_count / self.typing_speed_wpm
        seconds = minutes * 60
        seconds = seconds * random.uniform(0.8, 1.2)
        return seconds
    def _human_like_type(self, element, text):
        if not text:
            return
        element.click()
        element.press('Control+A')
        element.press('Backspace')
        total_time = self._calculate_typing_delay(text)
        per_char_time = total_time / len(text)
        typed_text = ''
        for i, char in enumerate(text):
            if random.random() < 0.01:
                typo_chars = 'qwertyuiopasdfghjklzxcvbnm'
                wrong_char = random.choice(typo_chars)
                element.type(wrong_char)
                typed_text += wrong_char
                self._simulate_human_delay(0.1, 0.3)
                element.press('Backspace')
                typed_text = typed_text[:-1]
            element.type(char)
            typed_text += char
            if random.random() < 0.05:
                self._simulate_human_delay(0.3, 1.0)
            else:
                char_delay = per_char_time * random.uniform(0.5, 1.5)
                time.sleep(char_delay)
    def _wait_for_page_stability(self):
        try:
            self.page.wait_for_load_state('networkidle', timeout=30000)
            self.page.wait_for_load_state('domcontentloaded', timeout=10000)
            self.page.wait_for_load_state('load', timeout=10000)
            self._simulate_human_delay(1.0, 2.0)
            return True
        except Exception as e:
            self.logger.warning(f'Page stability wait error: {str(e)}')
            return False
    def _load_memory(self):
        try:
            if os.path.exists(self.memory_file):
                with open(self.memory_file, 'r') as f:
                    self.conversation_history = json.load(f)
                self.logger.info(f'Loaded {len(self.conversation_history)} previous conversations')
        except Exception as e:
            self.logger.error(f'Error loading memory: {str(e)}')
    def _save_memory(self):
        try:
            with open(self.memory_file, 'w') as f:
                json.dump(self.conversation_history, f, indent=2)
        except Exception as e:
            self.logger.error(f'Error saving memory: {str(e)}')
    def click_by_vision(self, template_path, threshold=0.8):
        try:
            coords = self.find_element_by_image(template_path, threshold)
            if coords:
                x, y = coords
                self._human_like_mouse_movement(x, y)
                self.page.mouse.click(x, y)
                return True
            return False
        except Exception as e:
            self.logger.error(f'Error clicking by vision: {str(e)}')
            return False
    def _human_like_mouse_movement(self, target_x, target_y):
        try:
            current_pos = self.page.evaluate('() => { \n                return {x: window.mousePosX || 0, y: window.mousePosY || 0}\n            }')
            current_x = current_pos.get('x', 0)
            current_y = current_pos.get('y', 0)
            start_x, start_y = (current_x, current_y)
            end_x, end_y = (target_x, target_y)
            distance = ((end_x - start_x) ** 2 + (end_y - start_y) ** 2) ** 0.5
            steps = max(10, min(int(distance / 10), 100))
            control_x = (start_x + end_x) / 2 + random.uniform(-distance / 4, distance / 4)
            control_y = (start_y + end_y) / 2 + random.uniform(-distance / 4, distance / 4)
            for step in range(1, steps + 1):
                t = step / steps
                px = (1 - t) * (1 - t) * start_x + 2 * (1 - t) * t * control_x + t * t * end_x
                py = (1 - t) * (1 - t) * start_y + 2 * (1 - t) * t * control_y + t * t * end_y
                px += random.uniform(-2, 2)
                py += random.uniform(-2, 2)
                self.page.mouse.move(px, py)
                self.page.evaluate(f'() => {{ \n                    window.mousePosX = {px}; \n                    window.mousePosY = {py}; \n                }}')
                time.sleep(random.uniform(0.001, 0.01))
        except Exception as e:
            self.logger.warning(f'Error in human-like mouse movement: {str(e)}')
            self.page.mouse.move(target_x, target_y)
    def authenticate(self):
        try:
            username = os.getenv('CHAT_USERNAME')
            password = os.getenv('CHAT_PASSWORD')
            if not username or not password:
                self.logger.error('CHAT_USERNAME or CHAT_PASSWORD not found in environment')
                return False
            try:
                input_selectors = ['textarea[placeholder*="Message"]', 'textarea[placeholder*="Send a message"]', '[role="textbox"]', '#prompt-textarea']
                for selector in input_selectors:
                    try:
                        chat_input = self.page.wait_for_selector(selector, timeout=3000)
                        if chat_input:
                            self.logger.info('Already authenticated')
                            return True
                    except:
                        continue
            except:
                pass
            self.page.goto('https://auth0.openai.com/u/login/identifier', wait_until='networkidle')
            self.logger.info('Loaded login page')
            self._wait_for_page_stability()
            email_selectors = ['input[name="username"]', 'input[type="email"]', 'input[placeholder*="email" i]']
            email_input = None
            for selector in email_selectors:
                try:
                    email_input = self.page.wait_for_selector(selector, timeout=5000)
                    if email_input:
                        self.logger.info(f'Found email input with selector: {selector}')
                        break
                except:
                    continue
            if not email_input:
                if os.path.exists('templates/email_input.png'):
                    self.logger.info('Trying to find email input using vision')
                    coords = self.find_element_by_image('templates/email_input.png')
                    if coords:
                        x, y = coords
                        self.page.mouse.click(x, y)
                        email_input = self.page.wait_for_selector('input:focus')
            if not email_input:
                self.logger.error('Could not find email input field')
                self.page.screenshot(path='email_input_not_found.png')
                return False
            self._human_like_type(email_input, username)
            self.logger.info('Entered email')
            continue_button_selectors = ['button[type="submit"]', 'button:has-text("Continue")', 'button:has-text("Next")']
            continue_button = None
            for selector in continue_button_selectors:
                try:
                    continue_button = self.page.wait_for_selector(selector, timeout=3000)
                    if continue_button:
                        self.logger.info(f'Found continue button with selector: {selector}')
                        break
                except:
                    continue
            if not continue_button:
                if os.path.exists('templates/continue_button.png'):
                    self.logger.info('Trying to find continue button using vision')
                    if self.click_by_vision('templates/continue_button.png'):
                        continue_button = True
            if not continue_button:
                self.logger.error('Could not find continue button')
                self.page.screenshot(path='continue_button_not_found.png')
                return False
            if continue_button is not True:
                self._simulate_human_delay(0.5, 1.0)
                continue_button.click()
            self.logger.info('Clicked continue')
            self._wait_for_page_stability()
            password_selectors = ['input[name="password"]', 'input[type="password"]']
            password_input = None
            for selector in password_selectors:
                try:
                    password_input = self.page.wait_for_selector(selector, timeout=10000)
                    if password_input:
                        self.logger.info(f'Found password input with selector: {selector}')
                        break
                except:
                    continue
            if not password_input:
                if os.path.exists('templates/password_input.png'):
                    self.logger.info('Trying to find password input using vision')
                    coords = self.find_element_by_image('templates/password_input.png')
                    if coords:
                        x, y = coords
                        self.page.mouse.click(x, y)
                        password_input = self.page.wait_for_selector('input:focus')
            if not password_input:
                self.logger.error('Could not find password input field')
                self.page.screenshot(path='password_input_not_found.png')
                return False
            self._human_like_type(password_input, password)
            self.logger.info('Entered password')
            login_button_selectors = ['button[type="submit"]', 'button:has-text("Continue")', 'button:has-text("Log in")']
            login_button = None
            for selector in login_button_selectors:
                try:
                    login_button = self.page.wait_for_selector(selector, timeout=3000)
                    if login_button:
                        self.logger.info(f'Found login button with selector: {selector}')
                        break
                except:
                    continue
            if not login_button:
                if os.path.exists('templates/login_button.png'):
                    self.logger.info('Trying to find login button using vision')
                    if self.click_by_vision('templates/login_button.png'):
                        login_button = True
            if not login_button:
                self.logger.error('Could not find login button')
                self.page.screenshot(path='login_button_not_found.png')
                return False
            if login_button is not True:
                self._simulate_human_delay(0.5, 1.0)
                login_button.click()
            self.logger.info('Clicked login')
            chat_interface_found = False
            chat_input_selectors = ['[data-testid="chat-input"]', 'textarea[placeholder*="Message"]', 'textarea[placeholder*="Send a message"]', '[role="textbox"]', '#prompt-textarea']
            max_retries = 5
            for attempt in range(max_retries):
                try:
                    self._wait_for_page_stability()
                    if 'cloudflare' in self.page.url.lower() or 'challenge' in self.page.url.lower():
                        self.logger.info('Detected Cloudflare page, waiting...')
                        self.wait_for_cloudflare()
                        self._wait_for_page_stability()
                    for selector in chat_input_selectors:
                        try:
                            chat_input = self.page.wait_for_selector(selector, timeout=5000)
                            if chat_input:
                                self.logger.info(f'Successfully found chat interface using selector: {selector}')
                                chat_interface_found = True
                                break
                        except:
                            continue
                    if chat_interface_found:
                        break
                    self.logger.warning(f'Chat interface not found on attempt {attempt + 1}, waiting...')
                    self._simulate_human_delay(2.0, 5.0)
                except Exception as e:
                    self.logger.warning(f'Authentication check attempt {attempt + 1} error: {str(e)}')
                    self._simulate_human_delay(2.0, 5.0)
            if not chat_interface_found:
                self.logger.error('Failed to find chat interface after authentication')
                self.page.screenshot(path='chat_interface_not_found.png')
                return False
            self.logger.info('Successfully authenticated and found chat interface')
            return True
        except Exception as e:
            self.logger.error(f'Authentication error: {str(e)}')
            self.page.screenshot(path='auth_error.png')
            return False
    def wait_for_cloudflare(self, max_retries=3):
        try:
            self.logger.info('Checking for Cloudflare challenge...')
            if 'challenge' not in self.page.title().lower() and 'cloudflare' not in self.page.content().lower():
                return True
            self.logger.info('Detected Cloudflare challenge, waiting...')
            for attempt in range(max_retries):
                try:
                    self.page.wait_for_function("() => {\n                            return !document.title.toLowerCase().includes('cloudflare') && \n                                   !document.title.toLowerCase().includes('challenge') &&\n                                   !document.title.toLowerCase().includes('checking');\n                        }", timeout=30000)
                    self._wait_for_page_stability()
                    self.logger.info('Cloudflare challenge completed')
                    return True
                except Exception as e:
                    self.logger.warning(f'Cloudflare wait attempt {attempt + 1} failed: {str(e)}')
                    if attempt == max_retries - 1:
                        self.logger.info('Trying mouse movement to pass Cloudflare check')
                        for _ in range(5):
                            x = random.randint(100, 1000)
                            y = random.randint(100, 600)
                            self._human_like_mouse_movement(x, y)
                            self._simulate_human_delay(1.0, 3.0)
                            self.page.evaluate('window.scrollBy(0, 100)')
                            self._simulate_human_delay(1.0, 2.0)
                            self.page.evaluate('window.scrollBy(0, -50)')
                    if attempt < max_retries - 1:
                        self._simulate_human_delay(5.0, 10.0)
                    else:
                        raise
            return False
        except Exception as e:
            self.logger.error(f'Error waiting for Cloudflare: {str(e)}')
            self.page.screenshot(path='cloudflare_error.png')
            return False
    def send_message(self, message):
        max_retries = 3
        for attempt in range(max_retries):
            try:
                self.logger.info(f'Current URL before sending message: {self.page.url}')
                parsed_url = urlparse(self.page.url)
                if not (parsed_url.hostname == 'chat.openai.com' or parsed_url.hostname == 'chatgpt.com'):
                    self.logger.info('Not on chat page, navigating...')
                    self.page.goto('https://chat.openai.com', wait_until='networkidle')
                    self.logger.info(f'Navigated to: {self.page.url}')
                    if not self.wait_for_cloudflare():
                        if attempt < max_retries - 1:
                            self.logger.warning(f'Cloudflare challenge failed, attempt {attempt + 1}/{max_retries}')
                            self._simulate_human_delay(5.0, 10.0)
                            continue
                        else:
                            self.logger.error('Failed to pass Cloudflare challenge after all retries')
                            return None
                self.logger.info('Waiting for page to be ready...')
                self._wait_for_page_stability()
                input_selectors = ['textarea[placeholder*="Message"]', 'textarea[placeholder*="Send a message"]', '[role="textbox"]', 'div[contenteditable="true"]', '[data-testid="chat-input"]', '#prompt-textarea', 'div[class*="input"]', 'div[class*="chat"] textarea']
                chat_input = None
                for selector in input_selectors:
                    try:
                        self.logger.info(f'Trying selector: {selector}')
                        chat_input = self.page.wait_for_selector(selector, timeout=10000, state='visible')
                        if chat_input:
                            self.logger.info(f'Found input using selector: {selector}')
                            break
                    except Exception as e:
                        self.logger.info(f'Selector {selector} failed: {str(e)}')
                if not chat_input:
                    if os.path.exists('templates/chat_input.png'):
                        self.logger.info('Trying to find chat input using vision')
                        coords = self.find_element_by_image('templates/chat_input.png')
                        if coords:
                            x, y = coords
                            self.page.mouse.click(x, y)
                            chat_input = self.page.wait_for_selector('textarea:focus')
                if not chat_input:
                    if attempt < max_retries - 1:
                        self.logger.warning(f'Could not find chat input, attempt {attempt + 1}/{max_retries}')
                        self.page.screenshot(path=f'chat_input_error_{attempt + 1}.png')
                        self._simulate_human_delay(5.0, 10.0)
                        continue
                    else:
                        self.logger.error('Could not find chat input after all retries')
                        self.page.screenshot(path='no_input.png')
                        return None
                self._human_like_type(chat_input, message)
                self._simulate_human_delay(0.5, 1.0)
                if self.auto_save_enabled and self.chat_session_id:
                    try:
                        log_chat_message(session_id=self.chat_session_id, role='user', content=message, metadata={'url': self.page.url, 'platform': 'browser', 'timestamp': datetime.now().isoformat()})
                    except Exception as e:
                        self.logger.warning(f'Failed to log user message to session manager: {str(e)}')
                try:
                    chat_input.press('Enter')
                except Exception:
                    try:
                        send_button_selectors = ['button[aria-label="Send message"]', 'button svg[data-icon="paper-plane"]', 'button.send-button']
                        for selector in send_button_selectors:
                            try:
                                send_button = self.page.wait_for_selector(selector, timeout=3000)
                                if send_button:
                                    self._simulate_human_delay(0.2, 0.5)
                                    send_button.click()
                                    break
                            except:
                                continue
                    except Exception:
                        chat_input.press('Enter')
                message_time = datetime.now().isoformat()
                response_selectors = ['[data-message-author="assistant"]', '.message-content .markdown', '.assistant-message']
                response_elem = None
                response_text = None
                for selector in response_selectors:
                    try:
                        response_elem = self.page.wait_for_selector(selector, timeout=30000)
                        if response_elem:
                            self.logger.info(f'Found response using selector: {selector}')
                            break
                    except Exception as e:
                        self.logger.info(f'Response selector {selector} failed: {str(e)}')
                if not response_elem:
                    self.logger.warning('Could not detect response element')
                else:
                    try:
                        try:
                            self.page.wait_for_function('() => {\n                                    // Look for typing indicators or "thinking" states\n                                    return !document.querySelector(\'.typing-indicator\') &&\n                                           !document.querySelector(\'.loading-indicator\') &&\n                                           !document.querySelector(\'[data-state="thinking"]\');\n                                }', timeout=60000)
                        except Exception as e:
                            self.logger.warning(f'Could not detect response completion: {str(e)}')
                        response_text = response_elem.text_content()
                    except Exception as e:
                        self.logger.warning(f'Error getting response text: {str(e)}')
                conversation_entry = {'timestamp': message_time, 'message': message, 'response': response_text, 'url': self.page.url}
                self.conversation_history.append(conversation_entry)
                self._save_memory()
                if self.auto_save_enabled and self.chat_session_id and response_text:
                    try:
                        log_chat_message(session_id=self.chat_session_id, role='assistant', content=response_text, metadata={'url': self.page.url, 'platform': 'browser', 'timestamp': datetime.now().isoformat(), 'response_time': datetime.now().isoformat()})
                    except Exception as e:
                        self.logger.warning(f'Failed to log assistant response to session manager: {str(e)}')
                self.logger.info(f'Successfully processed message. Response length: {(len(response_text) if response_text else 0)}')
                self.last_action_time = time.time()
                return response_text
            except Exception as e:
                self.logger.error(f'Error in send_message attempt {attempt + 1}: {str(e)}')
                self.page.screenshot(path=f'send_error_{attempt + 1}.png')
                if attempt < max_retries - 1:
                    self._simulate_human_delay(5.0, 10.0)
                else:
                    return None
        return None
    def get_conversation_history(self):
        return self.conversation_history
    def get_last_response(self):
        if self.conversation_history:
            return self.conversation_history[-1].get('response')
        return None
    def capture_conversation_screenshot(self, filename=None):
        if filename is None:
            timestamp = datetime.now().strftime('%Y%m%d%H%M%S')
            filename = f'conversation_{timestamp}.png'
        try:
            self.page.screenshot(path=filename)
            self.logger.info(f'Captured conversation screenshot: {filename}')
            return filename
        except Exception as e:
            self.logger.error(f'Error capturing screenshot: {str(e)}')
            return None
    def clear_conversation(self):
        try:
            clear_chat_selectors = ['nav a:has-text("New chat")', 'button:has-text("New chat")', 'button:has-text("Clear chat")', 'button[aria-label="New chat"]']
            for selector in clear_chat_selectors:
                try:
                    button = self.page.wait_for_selector(selector, timeout=3000)
                    if button:
                        self._simulate_human_delay(0.5, 1.0)
                        button.click()
                        self._wait_for_page_stability()
                        self.logger.info('Cleared conversation')
                        return True
                except:
                    continue
            if os.path.exists('templates/new_chat_button.png'):
                if self.click_by_vision('templates/new_chat_button.png'):
                    self._wait_for_page_stability()
                    self.logger.info('Cleared conversation using vision')
                    return True
            self.logger.warning('Could not find clear conversation button')
            return False
        except Exception as e:
            self.logger.error(f'Error clearing conversation: {str(e)}')
            return False
    def browse_url_in_chat(self, url):
        browse_command = f'Please browse this URL and summarize the content: {url}'
        return self.send_message(browse_command)
    def create_echo_from_response(self, response_text):
        if not response_text:
            return None
        root = self.echo_system.create_tree('ChatGPT Response Root')
        paragraphs = response_text.split('\n\n')
        for paragraph in paragraphs:
            paragraph = paragraph.strip()
            if paragraph:
                child = TreeNode(content=paragraph, parent=root)
                root.children.append(child)
        self.echo_system.propagate_echoes()
        return self.echo_system.analyze_echo_patterns()
    def close(self):
        self._save_memory()
        if self.page:
            with contextlib.suppress(Exception):
                self.page.close()
        if self.browser:
            with contextlib.suppress(Exception):
                self.browser.close()
        if self.playwright:
            with contextlib.suppress(Exception):
                self.playwright.stop()
def main():
    chat = SeleniumInterface()
    if not chat.init():
        print('Failed to initialize browser')
        return
    if not chat.authenticate():
        print('Authentication failed')
        chat.close()
        return
    print('Successfully authenticated')
    response = chat.send_message('Tell me about Deep Tree Echo and Echo State Networks in a brief summary.')
    if response:
        print('\nResponse received:')
        print('-' * 50)
        print(response[:500] + '...' if len(response) > 500 else response)
        print('-' * 50)
        echo_patterns = chat.create_echo_from_response(response)
        if echo_patterns:
            print('\nEcho Patterns:')
            for key, value in echo_patterns.items():
                print(f'  {key}: {value}')
    chat.close()
if __name__ == '__main__':
    main()