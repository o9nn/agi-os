import logging
import time
import random
import numpy as np
import json
from pathlib import Path
from typing import Optional, Dict, Tuple, List, Union
import os
import subprocess
DISPLAY_AVAILABLE = 'DISPLAY' in os.environ
VIRTUAL_DISPLAY = DISPLAY_AVAILABLE and os.environ.get('DISPLAY', '').startswith(':')
HEADLESS = not DISPLAY_AVAILABLE
logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)
def create_xauth_file():
    xauth_path = os.path.expanduser('~/.Xauthority')
    if not os.path.exists(xauth_path):
        try:
            logger.info(f'Creating empty .Xauthority file at {xauth_path}')
            with open(xauth_path, 'wb'):
                pass
            return True
        except Exception as e:
            logger.error(f'Failed to create .Xauthority file: {str(e)}')
            return False
    return True
def create_x11_auth_cookie():
    if not DISPLAY_AVAILABLE:
        return False
    try:
        display = os.environ.get('DISPLAY', '')
        logger.info(f'Creating X11 auth cookie for display {display}')
        if display and display.startswith(':'):
            display[1:]
            cookie = subprocess.check_output('openssl rand -hex 16', shell=True).decode('utf-8').strip()
            xauth_path = os.path.expanduser('~/.Xauthority')
            cmd = f'touch {xauth_path} && xauth add {display} MIT-MAGIC-COOKIE-1 {cookie}'
            subprocess.run(cmd, shell=True, check=True)
            logger.info(f'Added auth cookie for display {display}')
            return True
    except Exception as e:
        logger.error(f'Error creating X11 auth cookie: {str(e)}')
        return False
def setup_x11_auth():
    if not DISPLAY_AVAILABLE:
        return False
    if not create_xauth_file():
        logger.error('Failed to create .Xauthority file')
        return False
    xauth_path = os.path.expanduser('~/.Xauthority')
    os.environ['XAUTHORITY'] = xauth_path
    try:
        if create_x11_auth_cookie():
            logger.info('X11 authentication set up successfully')
        else:
            logger.warning('X11 authentication cookie creation failed')
    except Exception as e:
        logger.warning(f'Could not create auth cookie: {str(e)}')
    return True
def ensure_display():
    global DISPLAY_AVAILABLE, VIRTUAL_DISPLAY, HEADLESS
    if DISPLAY_AVAILABLE:
        logger.info(f"Display detected: {os.environ.get('DISPLAY')}")
        setup_x11_auth()
        return True
    try:
        logger.info('No display detected, attempting to set up Xvfb virtual display')
        result = subprocess.run(['which', 'Xvfb'], capture_output=True, text=True)
        if result.returncode != 0:
            logger.error('Xvfb not found - unable to create virtual display')
            return False
        for display_num in range(99, 120):
            check_cmd = f'lsof -i :{6000 + display_num} || true'
            result = subprocess.run(check_cmd, shell=True, capture_output=True)
            if result.returncode != 0 or not result.stdout.strip():
                display = f':{display_num}'
                cmd = f'Xvfb {display} -screen 0 1920x1080x24 -ac +extension GLX +render -noreset &'
                subprocess.run(cmd, shell=True, check=True)
                time.sleep(1)
                os.environ['DISPLAY'] = display
                setup_x11_auth()
                DISPLAY_AVAILABLE = True
                VIRTUAL_DISPLAY = True
                HEADLESS = False
                logger.info(f'Virtual display created on {display}')
                return True
        logger.error('Failed to create virtual display - no available display numbers')
        return False
    except Exception as e:
        logger.error(f'Error setting up virtual display: {str(e)}')
        return False
DISPLAY_CONFIGURED = ensure_display()
CV2_AVAILABLE = False
PYAUTOGUI_AVAILABLE = False
PYNPUT_AVAILABLE = False
PIL_AVAILABLE = False
if DISPLAY_CONFIGURED:
    try:
        import cv2
        CV2_AVAILABLE = True
        logger.info('OpenCV imported successfully')
    except ImportError:
        logger.warning('Failed to import OpenCV (cv2)')
    try:
        import pyautogui
        PYAUTOGUI_AVAILABLE = True
        logger.info('PyAutoGUI imported successfully')
    except ImportError:
        logger.warning('Failed to import PyAutoGUI')
    try:
        from pynput import mouse, keyboard
        PYNPUT_AVAILABLE = True
        logger.info('Pynput imported successfully')
    except ImportError:
        logger.warning('Failed to import Pynput')
    try:
        from PIL import Image
        PIL_AVAILABLE = True
        logger.info('PIL imported successfully')
    except ImportError:
        logger.warning('Failed to import PIL')
try:
    from ml_system import MLSystem
    ML_AVAILABLE = True
except ImportError:
    logger.warning('Failed to import MLSystem')
    ML_AVAILABLE = False
class SensoryMotorSystem:
    def __init__(self):
        self.logger = logging.getLogger(__name__)
        self.headless = not DISPLAY_AVAILABLE
        self.echo_dir = Path.home() / '.deep_tree_echo'
        self.sensory_dir = self.echo_dir / 'sensory'
        self.sensory_dir.mkdir(parents=True, exist_ok=True)
        self.activity_file = self.sensory_dir / 'activity.json'
        self.activities = []
        self._load_activities()
        self.typing_speed = {'min': 0.1, 'max': 0.3, 'variance': 0.05}
        self.mouse_speed = {'min': 0.3, 'max': 2.0, 'variance': 0.1}
        self.spatial_awareness = {'depth_perception': 0.8, 'field_of_view': 110, 'peripheral_vision': 0.7, 'spatial_memory': 0.85, 'motion_tracking': 0.9}
        self.last_mouse_pos = self._get_mouse_position()
        self.last_action_time = time.time()
        self.frame_buffer = []
        self.frame_buffer_size = 5
        self.spatial_memory = {}
        self.ml = MLSystem() if ML_AVAILABLE else None
        if PYAUTOGUI_AVAILABLE:
            pyautogui.FAILSAFE = True
            pyautogui.PAUSE = 0.1
        self.logger.info(f'Sensory-Motor System initialized with 3D capabilities (headless={self.headless})')
    def _get_mouse_position(self) -> Tuple[int, int]:
        if not PYAUTOGUI_AVAILABLE:
            return (0, 0)
        try:
            return pyautogui.position()
        except Exception as e:
            self.logger.error(f'Error getting mouse position: {str(e)}')
            return (0, 0)
    def _load_activities(self):
        if self.activity_file.exists():
            try:
                with open(self.activity_file) as f:
                    self.activities = json.load(f)
            except:
                self.activities = []
    def _save_activities(self):
        with open(self.activity_file, 'w') as f:
            json.dump(self.activities[-1000:], f)
    def _log_activity(self, description: str, data: Optional[Dict]=None):
        activity = {'time': time.time(), 'description': description, 'data': data or {}}
        self.activities.append(activity)
        self._save_activities()
    async def process_all(self) -> Dict:
        self._log_activity('Processing sensory inputs')
        if DISPLAY_AVAILABLE:
            try:
                result = self.process_input()
                return result
            except Exception as e:
                self.logger.error(f'Error in process_all: {str(e)}')
                return {'status': 'error', 'message': str(e)}
        else:
            return {'status': 'skipped', 'reason': 'display unavailable'}
    def capture_screen(self, region=None) -> Union[np.ndarray, None]:
        if not PYAUTOGUI_AVAILABLE or not CV2_AVAILABLE:
            self._log_activity('Screen capture attempted but required libraries unavailable')
            return None
        try:
            screenshot = pyautogui.screenshot(region=region)
            frame = cv2.cvtColor(np.array(screenshot), cv2.COLOR_RGB2BGR)
            self.frame_buffer.append(frame)
            if len(self.frame_buffer) > self.frame_buffer_size:
                self.frame_buffer.pop(0)
            return frame
        except Exception as e:
            self.logger.error(f'Error capturing screen: {str(e)}')
            return None
    def process_input(self) -> Dict:
        if not DISPLAY_AVAILABLE:
            self._log_activity('Input processing skipped - display unavailable')
            return {'status': 'skipped', 'reason': 'display unavailable'}
        try:
            frame = self.capture_screen()
            results = {'status': 'processing'}
            if frame is not None:
                self._log_activity('Captured screen frame')
                if len(self.frame_buffer) >= 2:
                    motion_data = self.detect_motion()
                    if motion_data['motion_detected']:
                        self._log_activity('Motion detected', motion_data)
                        results['motion'] = motion_data
                if self.ml is not None:
                    objects = self.simulate_object_detection(frame)
                    if objects:
                        self._log_activity('Objects detected', {'objects': objects})
                        results['objects'] = objects
                        self.update_spatial_memory(objects)
            mouse_pos = self._get_mouse_position()
            if mouse_pos != self.last_mouse_pos:
                self._log_activity('Mouse movement', {'from': self.last_mouse_pos, 'to': mouse_pos})
                self.last_mouse_pos = mouse_pos
                results['mouse_moved'] = True
            results['status'] = 'processed'
            return results
        except Exception as e:
            self._log_activity('Error processing input', {'error': str(e)})
            self.logger.error(f'Error processing input: {str(e)}')
            return {'status': 'error', 'message': str(e)}
    def detect_motion(self) -> Dict:
        if len(self.frame_buffer) < 2 or not CV2_AVAILABLE:
            return {'motion_detected': False}
        try:
            prev_frame = self.frame_buffer[-2]
            curr_frame = self.frame_buffer[-1]
            prev_gray = cv2.cvtColor(prev_frame, cv2.COLOR_BGR2GRAY)
            curr_gray = cv2.cvtColor(curr_frame, cv2.COLOR_BGR2GRAY)
            frame_diff = cv2.absdiff(prev_gray, curr_gray)
            _, thresh = cv2.threshold(frame_diff, 25, 255, cv2.THRESH_BINARY)
            contours, _ = cv2.findContours(thresh, cv2.RETR_EXTERNAL, cv2.CHAIN_APPROX_SIMPLE)
            significant_contours = [c for c in contours if cv2.contourArea(c) > 100]
            motion_regions = []
            for contour in significant_contours:
                x, y, w, h = cv2.boundingRect(contour)
                motion_regions.append({'x': int(x), 'y': int(y), 'width': int(w), 'height': int(h)})
            motion_detected = len(significant_contours) > 0
            return {'motion_detected': motion_detected, 'motion_count': len(significant_contours), 'motion_regions': motion_regions}
        except Exception as e:
            self.logger.error(f'Error in motion detection: {str(e)}')
            return {'motion_detected': False, 'error': str(e)}
    def simulate_object_detection(self, frame: np.ndarray) -> List[Dict]:
        objects = []
        object_classes = ['player', 'wall', 'door', 'item', 'obstacle', 'enemy']
        for _ in range(random.randint(0, 5)):
            obj_class = random.choice(object_classes)
            obj_id = f'{obj_class}_{random.randint(1000, 9999)}'
            x = random.randint(0, frame.shape[1])
            y = random.randint(0, frame.shape[0])
            w = random.randint(20, 200)
            h = random.randint(20, 200)
            depth = random.uniform(1.0, 10.0)
            objects.append({'id': obj_id, 'class': obj_class, 'position': {'x': x, 'y': y, 'width': w, 'height': h}, 'depth': depth, 'confidence': random.uniform(0.7, 0.99)})
        return objects
    def update_spatial_memory(self, objects: List[Dict]):
        for obj in objects:
            obj_id = obj['id']
            position = obj['position']
            position['depth'] = obj['depth']
            x = position['x'] + position['width'] / 2
            y = position['y'] + position['height'] / 2
            z = position['depth']
            if obj_id in self.spatial_memory:
                prev_pos = self.spatial_memory[obj_id]['position']
                dx = x - prev_pos['x']
                dy = y - prev_pos['y']
                dz = z - prev_pos['z']
                velocity = {'dx': dx, 'dy': dy, 'dz': dz, 'speed': np.sqrt(dx * dx + dy * dy + dz * dz)}
                self.spatial_memory[obj_id] = {'position': {'x': x, 'y': y, 'z': z}, 'class': obj['class'], 'last_seen': time.time(), 'velocity': velocity}
            else:
                self.spatial_memory[obj_id] = {'position': {'x': x, 'y': y, 'z': z}, 'class': obj['class'], 'last_seen': time.time(), 'velocity': {'dx': 0, 'dy': 0, 'dz': 0, 'speed': 0}}
    def predict_object_position(self, obj_id: str, time_delta: float) -> Dict:
        if obj_id not in self.spatial_memory:
            return None
        obj = self.spatial_memory[obj_id]
        pos = obj['position']
        vel = obj['velocity']
        predicted_pos = {'x': pos['x'] + vel['dx'] * time_delta, 'y': pos['y'] + vel['dy'] * time_delta, 'z': pos['z'] + vel['dz'] * time_delta}
        return {'current': pos, 'predicted': predicted_pos, 'time_delta': time_delta}
    def simulate_depth_perception(self, frame: np.ndarray) -> np.ndarray:
        if not CV2_AVAILABLE:
            return None
        try:
            gray = cv2.cvtColor(frame, cv2.COLOR_BGR2GRAY)
            sobelx = cv2.Sobel(gray, cv2.CV_64F, 1, 0, ksize=3)
            sobely = cv2.Sobel(gray, cv2.CV_64F, 0, 1, ksize=3)
            magnitude = np.sqrt(sobelx ** 2 + sobely ** 2)
            magnitude = cv2.normalize(magnitude, None, 0, 1, cv2.NORM_MINMAX)
            depth_map = 1 - magnitude
            return depth_map
        except Exception as e:
            self.logger.error(f'Error in depth perception simulation: {str(e)}')
            return None
if __name__ == '__main__':
    print(f"X11 Display Status: {('Available' if DISPLAY_AVAILABLE else 'Not Available')}")
    print(f'Virtual Display: {VIRTUAL_DISPLAY}')
    print(f"Display: {os.environ.get('DISPLAY', 'Not Set')}")
    print(f'OpenCV Available: {CV2_AVAILABLE}')
    print(f'PyAutoGUI Available: {PYAUTOGUI_AVAILABLE}')
    print(f'Pynput Available: {PYNPUT_AVAILABLE}')
    print(f'PIL Available: {PIL_AVAILABLE}')
    print(f'ML System Available: {ML_AVAILABLE}')
    sensory = SensoryMotorSystem()
    print(f"Sensory Motor System initialized in {('headless' if sensory.headless else 'display')} mode")
    if CV2_AVAILABLE and PYAUTOGUI_AVAILABLE:
        print('Attempting to capture one frame...')
        frame = sensory.capture_screen()
        if frame is not None:
            print(f'Successfully captured frame with shape {frame.shape}')
            test_path = Path.home() / 'sensory_test_capture.png'
            cv2.imwrite(str(test_path), frame)
            print(f'Test image saved to {test_path}')
        else:
            print('Failed to capture frame')
    else:
        print('Frame capture not available - missing required libraries')