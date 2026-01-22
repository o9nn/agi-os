import cv2
import numpy as np
import pyautogui
import logging
import time
import random
from ml_system import MLSystem
import json
from pathlib import Path
from typing import Optional, Dict
class SensoryMotor:
    def __init__(self):
        self.logger = logging.getLogger(__name__)
        self.echo_dir = Path.home() / '.deep_tree_echo'
        self.sensory_dir = self.echo_dir / 'sensory'
        self.sensory_dir.mkdir(parents=True, exist_ok=True)
        self.activity_file = self.sensory_dir / 'activity.json'
        self.activities = []
        self._load_activities()
        pyautogui.FAILSAFE = True
        pyautogui.PAUSE = 0.1
        self.typing_speed = {'min': 0.1, 'max': 0.3, 'variance': 0.05}
        self.mouse_speed = {'min': 0.3, 'max': 2.0, 'variance': 0.1}
        self.last_mouse_pos = pyautogui.position()
        self.last_action_time = time.time()
        self.ml = MLSystem()
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
    def capture_screen(self, region=None):
        try:
            screenshot = pyautogui.screenshot(region=region)
            return cv2.cvtColor(np.array(screenshot), cv2.COLOR_RGB2BGR)
        except Exception as e:
            self.logger.error(f'Error capturing screen: {str(e)}')
            return None
    def find_element(self, template, threshold=0.8, region=None):
        try:
            screen = self.capture_screen(region)
            if screen is None:
                return None
            if isinstance(template, str):
                template = cv2.imread(template)
            result = cv2.matchTemplate(screen, template, cv2.TM_CCOEFF_NORMED)
            min_val, max_val, min_loc, max_loc = cv2.minMaxLoc(result)
            if max_val >= threshold:
                return {'confidence': max_val, 'location': max_loc, 'size': template.shape[:2]}
            return None
        except Exception as e:
            self.logger.error(f'Error finding element: {str(e)}')
            return None
    def move_mouse(self, x, y, duration=None, human_like=True):
        try:
            target_x = int(x)
            target_y = int(y)
            current_x, current_y = pyautogui.position()
            screen_width, screen_height = pyautogui.size()
            if target_x < 0 or target_x >= screen_width or target_y < 0 or (target_y >= screen_height):
                self.logger.error(f'Target position ({target_x}, {target_y}) is outside screen bounds (0, 0, {screen_width}, {screen_height})')
                return False
            margin = 5
            target_x = max(margin, min(screen_width - margin, target_x))
            target_y = max(margin, min(screen_height - margin, target_y))
            if human_like:
                points = self.ml.optimize_movement((current_x, current_y), (target_x, target_y))
                if not points:
                    points = self._generate_curve_points(current_x, current_y, target_x, target_y)
                if duration is None:
                    distance = np.sqrt((target_x - current_x) ** 2 + (target_y - current_y) ** 2)
                    base_duration = min(self.mouse_speed['max'], max(self.mouse_speed['min'], distance / 1000.0))
                    duration = base_duration + random.uniform(-self.mouse_speed['variance'], self.mouse_speed['variance'])
                start_time = time.time()
                for point in points:
                    x = max(margin, min(screen_width - margin, point[0]))
                    y = max(margin, min(screen_height - margin, point[1]))
                    pyautogui.moveTo(x, y, duration / len(points))
                end_time = time.time()
                self.ml.learn_from_interaction('mouse_movement', {'position': (current_x, current_y), 'time': start_time}, {'position': (target_x, target_y), 'time': end_time, 'duration': end_time - start_time, 'path': points}, True)
            else:
                pyautogui.moveTo(target_x, target_y, duration or self.mouse_speed['min'])
            self.last_mouse_pos = (target_x, target_y)
            self.last_action_time = time.time()
            return True
        except Exception as e:
            self.logger.error(f'Error moving mouse: {str(e)}')
            self.ml.learn_from_interaction('mouse_movement', {'position': (current_x, current_y)}, {'position': (target_x, target_y)}, False)
            return False
    def _generate_curve_points(self, start_x, start_y, end_x, end_y, points=10):
        curve_points = []
        control_x = random.uniform(min(start_x, end_x), max(start_x, end_x))
        control_y = random.uniform(min(start_y, end_y), max(start_y, end_y))
        for i in range(points):
            t = i / (points - 1)
            x = (1 - t) ** 2 * start_x + 2 * (1 - t) * t * control_x + t ** 2 * end_x
            y = (1 - t) ** 2 * start_y + 2 * (1 - t) * t * control_y + t ** 2 * end_y
            curve_points.append((int(x), int(y)))
        return curve_points
    def click(self, button='left', clicks=1, interval=0.2):
        try:
            for _ in range(clicks):
                pyautogui.click(button=button)
                if clicks > 1:
                    time.sleep(interval + random.uniform(-self.mouse_speed['variance'], self.mouse_speed['variance']))
            self.last_action_time = time.time()
        except Exception as e:
            self.logger.error(f'Error clicking: {str(e)}')
    def wait_for_element(self, template, timeout=30, interval=0.5, threshold=0.8):
        try:
            start_time = time.time()
            while time.time() - start_time < timeout:
                screen = self.capture_screen()
                if screen is None:
                    continue
                element = self.ml.detect_element(screen, template, threshold)
                if element:
                    return element
                time.sleep(interval)
            return None
        except Exception as e:
            self.logger.error(f'Error waiting for element: {str(e)}')
            return None
    def type_text(self, text, interval=None):
        try:
            start_time = time.time()
            if interval is None:
                base_interval = random.uniform(self.typing_speed['min'], self.typing_speed['max'])
            else:
                base_interval = interval
            typed_chars = []
            for char in text:
                current_interval = base_interval + random.uniform(-self.typing_speed['variance'], self.typing_speed['variance'])
                pyautogui.typewrite(char, interval=current_interval)
                typed_chars.append(char)
                if random.random() < 0.1:
                    time.sleep(random.uniform(0.5, 1.0))
            end_time = time.time()
            self.ml.learn_from_interaction('typing', {'time': start_time, 'text_length': len(text)}, {'time': end_time, 'duration': end_time - start_time, 'chars_typed': len(typed_chars)}, True)
            self.last_action_time = time.time()
        except Exception as e:
            self.logger.error(f'Error typing text: {str(e)}')
            self.ml.learn_from_interaction('typing', {'text_length': len(text)}, {'chars_typed': 0}, False)
    def scroll(self, clicks, direction='down'):
        try:
            actual_clicks = clicks + random.randint(-1, 1)
            if direction == 'down':
                pyautogui.scroll(-actual_clicks)
            else:
                pyautogui.scroll(actual_clicks)
            time.sleep(random.uniform(0.1, 0.3))
            self.last_action_time = time.time()
        except Exception as e:
            self.logger.error(f'Error scrolling: {str(e)}')
    def drag_and_drop(self, start_x, start_y, end_x, end_y, duration=None):
        try:
            self.move_mouse(start_x, start_y)
            time.sleep(random.uniform(0.1, 0.3))
            pyautogui.mouseDown()
            time.sleep(random.uniform(0.1, 0.2))
            self.move_mouse(end_x, end_y, duration)
            time.sleep(random.uniform(0.1, 0.2))
            pyautogui.mouseUp()
            self.last_action_time = time.time()
        except Exception as e:
            self.logger.error(f'Error performing drag and drop: {str(e)}')
    def hover(self, x, y, duration=1.0):
        try:
            self.move_mouse(x, y)
            actual_duration = duration + random.uniform(-self.mouse_speed['variance'], self.mouse_speed['variance'])
            time.sleep(actual_duration)
            self.last_action_time = time.time()
        except Exception as e:
            self.logger.error(f'Error hovering: {str(e)}')
    def process_input(self):
        try:
            screenshot = self.capture_screen()
            if screenshot is not None:
                self._log_activity('Captured screen')
            mouse_pos = pyautogui.position()
            if mouse_pos != self.last_mouse_pos:
                self._log_activity('Mouse movement', {'from': self.last_mouse_pos, 'to': mouse_pos})
                self.last_mouse_pos = mouse_pos
        except Exception as e:
            self._log_activity('Error processing input', {'error': str(e)})
            self.logger.error(f'Error processing input: {str(e)}')
            return None