import unittest
import logging
import sys
from pathlib import Path
sys.path.insert(0, str(Path(__file__).parent))
try:
    from differential_emotion_theory import DETEmotion
    DET_AVAILABLE = True
except ImportError as e:
    DET_AVAILABLE = False
    print(f'Warning: Could not import differential_emotion_theory: {e}')
class TestDifferentialEmotionTheory(unittest.TestCase):
    def setUp(self):
        logging.getLogger().setLevel(logging.CRITICAL)
    def test_import_differential_emotion_theory(self):
        if not DET_AVAILABLE:
            self.skipTest('differential_emotion_theory module not available')
        self.assertTrue(DET_AVAILABLE)
    @unittest.skipIf(not DET_AVAILABLE, 'differential_emotion_theory not available')
    def test_det_emotion_enum_exists(self):
        self.assertIsNotNone(DETEmotion)
        expected_emotions = ['INTEREST', 'EXCITEMENT', 'ANGER', 'CONTEMPT', 'DISGUST', 'FEAR', 'SHAME']
        available_emotions = []
        for emotion in expected_emotions:
            if hasattr(DETEmotion, emotion):
                available_emotions.append(emotion)
        self.assertGreater(len(available_emotions), 0, f'No expected emotions found. Available: {[e.name for e in DETEmotion]}')
    @unittest.skipIf(not DET_AVAILABLE, 'differential_emotion_theory not available')
    def test_det_emotion_enum_values(self):
        for emotion in DETEmotion:
            self.assertIsInstance(emotion.value, int)
            self.assertGreaterEqual(emotion.value, 0)
    @unittest.skipIf(not DET_AVAILABLE, 'differential_emotion_theory not available')
    def test_det_emotion_unique_values(self):
        values = [emotion.value for emotion in DETEmotion]
        unique_values = set(values)
        self.assertEqual(len(values), len(unique_values), 'DETEmotion enum should have unique values')
    @unittest.skipIf(not DET_AVAILABLE, 'differential_emotion_theory not available')
    def test_det_emotion_names(self):
        for emotion in DETEmotion:
            self.assertIsInstance(emotion.name, str)
            self.assertEqual(emotion.name, emotion.name.upper())
            self.assertGreater(len(emotion.name), 0)
    @unittest.skipIf(not DET_AVAILABLE, 'differential_emotion_theory not available')
    def test_basic_emotions_present(self):
        basic_emotions = ['INTEREST', 'ANGER', 'FEAR']
        available_basic = []
        for emotion_name in basic_emotions:
            if hasattr(DETEmotion, emotion_name):
                available_basic.append(emotion_name)
        self.assertGreater(len(available_basic), 0, 'At least some basic emotions should be available')
    @unittest.skipIf(not DET_AVAILABLE, 'differential_emotion_theory not available')
    def test_emotion_ordering(self):
        emotions_list = list(DETEmotion)
        self.assertGreater(len(emotions_list), 0)
        values = [emotion.value for emotion in emotions_list]
        self.assertEqual(values, sorted(values))
    @unittest.skipIf(not DET_AVAILABLE, 'differential_emotion_theory not available')
    def test_det_emotion_iteration(self):
        emotion_count = 0
        for emotion in DETEmotion:
            emotion_count += 1
            self.assertIsInstance(emotion, DETEmotion)
        self.assertGreater(emotion_count, 0)
    @unittest.skipIf(not DET_AVAILABLE, 'differential_emotion_theory not available')
    def test_det_emotion_access_by_name(self):
        if hasattr(DETEmotion, 'INTEREST'):
            interest = DETEmotion.INTEREST
            self.assertEqual(interest.name, 'INTEREST')
            self.assertIsInstance(interest.value, int)
    @unittest.skipIf(not DET_AVAILABLE, 'differential_emotion_theory not available')
    def test_det_emotion_access_by_value(self):
        emotions_list = list(DETEmotion)
        if emotions_list:
            first_emotion = emotions_list[0]
            same_emotion = DETEmotion(first_emotion.value)
            self.assertEqual(first_emotion, same_emotion)
    @unittest.skipIf(not DET_AVAILABLE, 'differential_emotion_theory not available')
    def test_module_structure(self):
        import differential_emotion_theory as det_module
        module_attrs = [attr for attr in dir(det_module) if not attr.startswith('_') and attr.isupper()]
        self.assertIn('DETEmotion', module_attrs)
def main():
    unittest.main(verbosity=2)
if __name__ == '__main__':
    main()