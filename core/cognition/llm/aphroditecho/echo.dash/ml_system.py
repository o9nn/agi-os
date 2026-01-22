import numpy as np
import logging
from pathlib import Path
import pickle
import json
from datetime import datetime
from typing import Dict, List, Tuple, Optional, Any
import time
try:
    import tensorflow as tf
    from tensorflow.keras import layers, models
    TENSORFLOW_AVAILABLE = True
except ImportError:
    TENSORFLOW_AVAILABLE = False
    tf = None
    layers = None
    models = None
try:
    import cv2
    CV2_AVAILABLE = True
except ImportError:
    CV2_AVAILABLE = False
    cv2 = None
try:
    from PIL import Image
    PIL_AVAILABLE = True
except ImportError:
    PIL_AVAILABLE = False
    Image = None
class FallbackModel:
    def __init__(self, model_type: str):
        self.model_type = model_type
        self.logger = logging.getLogger(__name__)
        self.training_data = []
    def predict(self, data):
        if isinstance(data, np.ndarray):
            if len(data.shape) == 1:
                return np.array([np.mean(data) * 0.7 + 0.3])
            else:
                return np.mean(data, axis=0) * 0.8 + 0.2
        else:
            return np.array([0.5])
    def fit(self, x, y):
        self.training_data.append((x, y))
        return self
    def save(self, path):
        try:
            with open(f'{path}_fallback.pkl', 'wb') as f:
                pickle.dump(self.training_data, f)
        except Exception as e:
            self.logger.error(f'Error saving fallback model: {e}')
    def load(self, path):
        try:
            with open(f'{path}_fallback.pkl', 'rb') as f:
                self.training_data = pickle.load(f)
        except Exception as e:
            self.logger.warning(f'Could not load fallback model: {e}')
class MLSystem:
    def __init__(self):
        self.logger = logging.getLogger(__name__)
        self.echo_dir = Path.home() / '.deep_tree_echo'
        self.ml_dir = self.echo_dir / 'ml'
        self.ml_dir.mkdir(parents=True, exist_ok=True)
        self.activity_file = self.ml_dir / 'activity.json'
        self.activities = []
        self._load_activities()
        self.models_dir = Path.home() / '.deep_tree_echo' / 'models'
        self.models_dir.mkdir(parents=True, exist_ok=True)
        self.visual_model = None
        self.behavior_model = None
        self.pattern_model = None
        self.echo_value_model = None
        self.interaction_history = []
        self._load_models()
    def _load_models(self):
        if not TENSORFLOW_AVAILABLE:
            self.logger.warning('TensorFlow not available - using fallback mode')
            self._create_fallback_models()
            return
        try:
            visual_path = self.models_dir / 'visual_model'
            if visual_path.exists():
                self.visual_model = models.load_model(visual_path)
            else:
                self.visual_model = self._create_visual_model()
            behavior_path = self.models_dir / 'behavior_model'
            if behavior_path.exists():
                self.behavior_model = models.load_model(behavior_path)
            else:
                self.behavior_model = self._create_behavior_model()
            pattern_path = self.models_dir / 'pattern_model'
            if pattern_path.exists():
                self.pattern_model = models.load_model(pattern_path)
            else:
                self.pattern_model = self._create_pattern_model()
            echo_value_path = self.models_dir / 'echo_value_model'
            if echo_value_path.exists():
                self.echo_value_model = models.load_model(echo_value_path)
            else:
                self.echo_value_model = self._create_echo_value_model()
            self.logger.info('Successfully loaded ML models')
        except Exception as e:
            self.logger.error(f'Error loading models: {str(e)}')
            self._create_fallback_models()
    def _create_fallback_models(self):
        self.logger.info('Creating fallback models (no TensorFlow)')
        self.visual_model = FallbackModel('visual')
        self.behavior_model = FallbackModel('behavior')
        self.pattern_model = FallbackModel('pattern')
        self.echo_value_model = FallbackModel('echo_value')
        self.logger.info('Fallback models created successfully')
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
    def _create_visual_model(self):
        if not TENSORFLOW_AVAILABLE:
            return FallbackModel('visual')
        model = models.Sequential([layers.Input(shape=(224, 224, 3)), layers.Conv2D(64, 3, activation='relu', padding='same'), layers.MaxPooling2D(), layers.Conv2D(128, 3, activation='relu', padding='same'), layers.MaxPooling2D(), layers.Conv2D(128, 3, activation='relu', padding='same'), layers.MaxPooling2D(), layers.Flatten(), layers.Dense(128, activation='relu'), layers.Dense(64, activation='relu'), layers.Dense(4)])
        model.compile(optimizer='adam', loss='mse', metrics=['mae'])
        return model
    def _create_behavior_model(self):
        if not TENSORFLOW_AVAILABLE:
            return FallbackModel('behavior')
        model = models.Sequential([layers.Input(shape=(4,)), layers.Dense(64, activation='relu'), layers.Dropout(0.2), layers.Dense(32, activation='relu'), layers.Dense(16, activation='relu'), layers.Dense(8)])
        model.compile(optimizer='adam', loss='mse', metrics=['mae'])
        return model
    def _create_pattern_model(self):
        if not TENSORFLOW_AVAILABLE:
            return FallbackModel('pattern')
        model = models.Sequential([layers.Input(shape=(100,)), layers.Dense(64, activation='relu'), layers.Dropout(0.2), layers.Dense(32, activation='relu'), layers.Dense(16, activation='relu'), layers.Dense(8)])
        model.compile(optimizer='adam', loss='mse', metrics=['mae'])
        return model
    def _create_echo_value_model(self):
        if not TENSORFLOW_AVAILABLE:
            return FallbackModel('echo_value')
        model = models.Sequential([layers.Input(shape=(6,)), layers.Dense(64, activation='relu'), layers.Dropout(0.2), layers.Dense(32, activation='relu'), layers.Dense(16, activation='relu'), layers.Dense(1)])
        model.compile(optimizer='adam', loss='mse', metrics=['mae'])
        return model
    def detect_element(self, screenshot: np.ndarray, template: np.ndarray, threshold: float=0.8) -> Optional[Dict]:
        if not CV2_AVAILABLE:
            self.logger.warning('CV2 not available - using fallback element detection')
            return self._fallback_detect_element(screenshot, template, threshold)
        try:
            if screenshot is None or template is None:
                self.logger.error('Invalid input: screenshot or template is None')
                return None
            if len(screenshot.shape) == 2:
                screenshot = cv2.cvtColor(screenshot, cv2.COLOR_GRAY2RGB)
            elif screenshot.shape[2] == 4:
                screenshot = cv2.cvtColor(screenshot, cv2.COLOR_BGRA2RGB)
            elif screenshot.shape[2] == 3 and screenshot.dtype == np.uint8:
                screenshot = cv2.cvtColor(screenshot, cv2.COLOR_BGR2RGB)
            resized = cv2.resize(screenshot, (224, 224))
            normalized = resized.astype(np.float32) / 255.0
            model_input = np.expand_dims(normalized, axis=0)
            prediction = self.visual_model.predict(model_input, verbose=0)
            x, y, w, h = prediction[0]
            x = int(x * screenshot.shape[1])
            y = int(y * screenshot.shape[0])
            w = int(w * screenshot.shape[1])
            h = int(h * screenshot.shape[0])
            if w <= 0 or h <= 0 or x < 0 or (y < 0):
                result = cv2.matchTemplate(screenshot, template, cv2.TM_CCOEFF_NORMED)
                min_val, max_val, min_loc, max_loc = cv2.minMaxLoc(result)
                if max_val >= threshold:
                    return {'confidence': float(max_val), 'location': max_loc, 'size': template.shape[:2]}
            else:
                return {'confidence': 1.0, 'location': (x, y), 'size': (w, h)}
            return None
        except Exception as e:
            self.logger.error(f'Error detecting element: {str(e)}')
            return None
    def optimize_movement(self, start_pos: Tuple[int, int], end_pos: Tuple[int, int]) -> List[Tuple[int, int]]:
        try:
            features = np.array([start_pos[0], start_pos[1], end_pos[0], end_pos[1]]).reshape(1, -1)
            control_points = self.behavior_model.predict(features, verbose=0)[0]
            distance = np.linalg.norm(np.array(end_pos) - np.array(start_pos))
            num_points = max(int(distance / 10), 5)
            points = []
            for i in range(num_points):
                t = i / (num_points - 1)
                variation_x = control_points[i % 4] * 0.1
                variation_y = control_points[(i + 4) % 8] * 0.1
                x = int(start_pos[0] + (end_pos[0] - start_pos[0]) * t + np.sin(t * np.pi) * variation_x * distance)
                y = int(start_pos[1] + (end_pos[1] - start_pos[1]) * t + np.sin(t * np.pi) * variation_y * distance)
                points.append((x, y))
            return points
        except Exception as e:
            self.logger.error(f'Error optimizing movement: {str(e)}')
            num_points = max(int(np.linalg.norm(np.array(end_pos) - np.array(start_pos)) / 10), 5)
            points = []
            for i in range(num_points):
                t = i / (num_points - 1)
                x = int(start_pos[0] + (end_pos[0] - start_pos[0]) * t)
                y = int(start_pos[1] + (end_pos[1] - start_pos[1]) * t)
                points.append((x, y))
            return points
    def learn_from_interaction(self, interaction_type: str, start_state: Dict, end_state: Dict, success: bool):
        try:
            interaction = {'type': interaction_type, 'start_state': start_state, 'end_state': end_state, 'success': success, 'timestamp': datetime.now().isoformat()}
            self.interaction_history.append(interaction)
            if len(self.interaction_history) % 100 == 0:
                self._save_interaction_history()
            if len(self.interaction_history) >= 1000:
                self._update_behavior_model()
        except Exception as e:
            self.logger.error(f'Error learning from interaction: {str(e)}')
    def create_continuous_learning_interaction(self, interaction_type: str, start_state: Dict, end_state: Dict, success: bool, performance_score: Optional[float]=None) -> 'InteractionData':
        try:
            from datetime import datetime
            interaction_id = f'{interaction_type}_{len(self.interaction_history):06d}'
            if performance_score is None:
                if success:
                    performance_score = 0.7
                    if 'duration' in end_state and end_state['duration'] < 1.0:
                        performance_score += 0.1
                    if 'accuracy' in end_state and end_state['accuracy'] > 0.8:
                        performance_score += 0.2
                    performance_score = min(performance_score, 1.0)
                else:
                    performance_score = -0.3
                    if 'partial_success' in end_state and end_state['partial_success']:
                        performance_score = -0.1
            context_metadata = {'ml_system_generated': True, 'interaction_duration': end_state.get('duration', 0.0), 'task_complexity': self._estimate_task_complexity(start_state, end_state), 'learning_context': interaction_type}
            if interaction_type in ['reasoning', 'memory_recall', 'complex_task']:
                context_metadata['importance'] = 0.8
            elif interaction_type in ['simple_task', 'routine']:
                context_metadata['importance'] = 0.3
            else:
                context_metadata['importance'] = 0.5
            interaction_data = {'interaction_id': interaction_id, 'interaction_type': interaction_type, 'input_data': {'start_state': start_state, 'context': context_metadata}, 'output_data': {'end_state': end_state, 'success': success}, 'performance_feedback': performance_score, 'timestamp': datetime.now(), 'context_metadata': context_metadata, 'success': success}
            self.logger.debug(f'Created continuous learning interaction: {interaction_id}, performance={performance_score:.3f}, success={success}')
            return interaction_data
        except Exception as e:
            self.logger.error(f'Error creating continuous learning interaction: {str(e)}')
            return {'interaction_id': f'error_{len(self.interaction_history)}', 'interaction_type': interaction_type, 'input_data': start_state, 'output_data': end_state, 'performance_feedback': -0.5 if not success else 0.5, 'timestamp': datetime.now(), 'context_metadata': {'error': str(e)}, 'success': success}
    def _estimate_task_complexity(self, start_state: Dict, end_state: Dict) -> float:
        try:
            complexity_factors = []
            if 'position' in start_state and 'position' in end_state:
                start_pos = start_state['position']
                end_pos = end_state['position']
                if isinstance(start_pos, (list, tuple)) and isinstance(end_pos, (list, tuple)):
                    distance = sum(((e - s) ** 2 for s, e in zip(start_pos, end_pos))) ** 0.5
                    complexity_factors.append(min(distance / 1000.0, 1.0))
            if 'path' in end_state and isinstance(end_state['path'], list):
                path_length = len(end_state['path'])
                complexity_factors.append(min(path_length / 100.0, 1.0))
            if 'duration' in end_state:
                duration = end_state['duration']
                complexity_factors.append(min(duration / 10.0, 1.0))
            def estimate_data_size(data):
                if isinstance(data, dict):
                    return len(str(data))
                elif isinstance(data, (list, tuple)):
                    return len(data)
                else:
                    return len(str(data))
            input_size = estimate_data_size(start_state)
            output_size = estimate_data_size(end_state)
            size_complexity = (input_size + output_size) / 10000.0
            complexity_factors.append(min(size_complexity, 1.0))
            if complexity_factors:
                return sum(complexity_factors) / len(complexity_factors)
            else:
                return 0.5
        except Exception as e:
            self.logger.warning(f'Error estimating task complexity: {str(e)}')
            return 0.5
    async def learn_continuously(self, continuous_learning_system, interaction_type: str, start_state: Dict, end_state: Dict, success: bool, performance_score: Optional[float]=None) -> Dict[str, Any]:
        try:
            interaction_data = self.create_continuous_learning_interaction(interaction_type, start_state, end_state, success, performance_score)
            self.learn_from_interaction(interaction_type, start_state, end_state, success)
            if hasattr(continuous_learning_system, 'learn_from_interaction'):
                if isinstance(interaction_data, dict):
                    try:
                        from aphrodite.continuous_learning import InteractionData
                        from datetime import datetime
                        interaction_obj = InteractionData(interaction_id=interaction_data['interaction_id'], interaction_type=interaction_data['interaction_type'], input_data=interaction_data['input_data'], output_data=interaction_data['output_data'], performance_feedback=interaction_data['performance_feedback'], timestamp=interaction_data['timestamp'], context_metadata=interaction_data['context_metadata'], success=interaction_data['success'])
                        interaction_data = interaction_obj
                    except ImportError:
                        self.logger.warning('ContinuousLearning not available, using traditional learning only')
                        return {'success': True, 'method': 'traditional_only'}
                cl_result = await continuous_learning_system.learn_from_interaction(interaction_data)
                self.logger.info(f"Continuous learning applied: success={cl_result.get('success', False)}, interaction={(interaction_data.interaction_id if hasattr(interaction_data, 'interaction_id') else 'unknown')}")
                return {'success': cl_result.get('success', False), 'method': 'continuous_learning', 'continuous_result': cl_result, 'traditional_learning': True}
            else:
                self.logger.warning('Continuous learning system not properly configured')
                return {'success': True, 'method': 'traditional_only'}
        except Exception as e:
            self.logger.error(f'Error in continuous learning: {str(e)}')
            return {'success': False, 'error': str(e), 'method': 'failed'}
    def _save_interaction_history(self):
        try:
            history_path = self.models_dir / 'interaction_history.json'
            with open(history_path, 'w') as f:
                json.dump(self.interaction_history[-1000:], f)
        except Exception as e:
            self.logger.error(f'Error saving interaction history: {str(e)}')
    def _update_behavior_model(self):
        try:
            successful_interactions = [i for i in self.interaction_history if i['success'] and i['type'] == 'mouse_movement']
            if len(successful_interactions) < 100:
                return
            X = []
            y = []
            for interaction in successful_interactions[-1000:]:
                start = interaction['start_state'].get('position')
                end = interaction['end_state'].get('position')
                path = interaction['end_state'].get('path', [])
                if start and end and path:
                    X.append([start[0], start[1], end[0], end[1]])
                    control_points = []
                    for i in range(0, len(path), len(path) // 8):
                        if len(control_points) < 8:
                            point = path[i]
                            control_points.append((point[0] - start[0]) / 10)
                            control_points.append((point[1] - start[1]) / 10)
                    y.append(control_points[:8])
            if not X:
                return
            X = np.array(X)
            y = np.array(y)
            self.behavior_model.fit(X, y, epochs=10, batch_size=32, verbose=0)
            self.behavior_model.save(self.models_dir / 'behavior_model')
            self.logger.info('Successfully updated behavior model')
        except Exception as e:
            self.logger.error(f'Error updating behavior model: {str(e)}')
    def analyze_patterns(self, interactions: List[Dict]) -> Dict:
        try:
            patterns = {'timing': {}, 'movement': {}, 'success_rate': {}}
            timestamps = [datetime.fromisoformat(i['timestamp']) for i in interactions]
            if len(timestamps) > 1:
                intervals = [(timestamps[i + 1] - timestamps[i]).total_seconds() for i in range(len(timestamps) - 1)]
                patterns['timing']['mean_interval'] = np.mean(intervals)
                patterns['timing']['std_interval'] = np.std(intervals)
            movements = [i for i in interactions if i['type'] == 'mouse_movement']
            if movements:
                distances = []
                speeds = []
                for m in movements:
                    start = m['start_state'].get('position')
                    end = m['end_state'].get('position')
                    if start and end:
                        distance = np.sqrt((end[0] - start[0]) ** 2 + (end[1] - start[1]) ** 2)
                        duration = float(m['end_state'].get('duration', 1.0))
                        distances.append(distance)
                        speeds.append(distance / duration)
                patterns['movement']['mean_distance'] = np.mean(distances)
                patterns['movement']['mean_speed'] = np.mean(speeds)
            total = len(interactions)
            successful = len([i for i in interactions if i['success']])
            patterns['success_rate']['overall'] = successful / total
            return patterns
        except Exception as e:
            self.logger.error(f'Error analyzing patterns: {str(e)}')
            return {}
    async def update_models(self):
        self._log_activity('Starting model update')
        try:
            self._log_activity('Updating visual model')
            await self.visual_model.update()
            self._log_activity('Updating behavior model')
            await self.behavior_model.update()
            self._log_activity('Updating pattern model')
            await self.pattern_model.update()
            self._log_activity('Updating echo value model')
            await self.echo_value_model.update()
            self._log_activity('Model update complete')
        except Exception as e:
            self._log_activity('Error updating models', {'error': str(e)})
            raise
    def predict_echo_value(self, features: List[float]) -> float:
        try:
            features = np.array(features).reshape(1, -1)
            prediction = self.echo_value_model.predict(features, verbose=0)
            return float(prediction[0][0])
        except Exception as e:
            self.logger.error(f'Error predicting echo value: {str(e)}')
            return 0.0
    def _fallback_detect_element(self, screenshot: np.ndarray, template: np.ndarray, threshold: float=0.8) -> Optional[Dict]:
        try:
            if screenshot.shape[:2] != template.shape[:2]:
                h, w = screenshot.shape[:2]
                return {'confidence': 0.5, 'location': (w // 2, h // 2), 'size': template.shape[:2]}
            screenshot_flat = screenshot.flatten().astype(float)
            template_flat = template.flatten().astype(float)
            screenshot_norm = (screenshot_flat - np.mean(screenshot_flat)) / np.std(screenshot_flat)
            template_norm = (template_flat - np.mean(template_flat)) / np.std(template_flat)
            correlation = np.corrcoef(screenshot_norm, template_norm)[0, 1]
            if correlation >= threshold:
                return {'confidence': float(correlation), 'location': (0, 0), 'size': template.shape[:2]}
            return None
        except Exception as e:
            self.logger.error(f'Error in fallback element detection: {e}')
            return None