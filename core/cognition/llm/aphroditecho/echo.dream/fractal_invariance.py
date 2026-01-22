import numpy as np
import matplotlib.pyplot as plt
from io import BytesIO
import base64
class FractalInvariance:
    def __init__(self):
        self.scale_levels = 5
        self.symmetry_types = ['reflection', 'rotation', 'translation', 'dilation']
    def generate_invariant_pattern(self, pattern_type='sierpinski', iterations=5, symmetry=None):
        plt.figure(figsize=(8, 8))
        if pattern_type == 'sierpinski':
            self._generate_sierpinski(iterations, symmetry)
        elif pattern_type == 'mandelbrot':
            self._generate_mandelbrot(iterations, symmetry)
        elif pattern_type == 'koch':
            self._generate_koch(iterations, symmetry)
        else:
            self._generate_recursive_squares(iterations, symmetry)
        buffer = BytesIO()
        plt.savefig(buffer, format='png')
        buffer.seek(0)
        image_base64 = base64.b64encode(buffer.getvalue()).decode('utf-8')
        plt.close()
        return image_base64
    def analyze_symmetry(self, data):
        if not isinstance(data, np.ndarray):
            data = np.array(data)
        results = {'reflection_symmetry': False, 'rotational_symmetry': False, 'scale_invariance': False, 'symmetry_score': 0.0, 'invariance_patterns': []}
        if len(data.shape) == 1:
            reverse = data[::-1]
            reflection_score = 1 - np.sum(np.abs(data - reverse)) / (np.sum(np.abs(data)) + 1e-10)
            results['reflection_symmetry'] = reflection_score > 0.8
            results['symmetry_score'] += reflection_score
        elif len(data.shape) == 2:
            h_reflection = data[::-1, :]
            h_score = 1 - np.sum(np.abs(data - h_reflection)) / (np.sum(np.abs(data)) + 1e-10)
            v_reflection = data[:, ::-1]
            v_score = 1 - np.sum(np.abs(data - v_reflection)) / (np.sum(np.abs(data)) + 1e-10)
            reflection_score = max(h_score, v_score)
            results['reflection_symmetry'] = reflection_score > 0.8
            results['symmetry_score'] += reflection_score
        if len(data.shape) == 2 and data.shape[0] == data.shape[1]:
            rot90 = np.rot90(data)
            rot180 = np.rot90(data, 2)
            rot270 = np.rot90(data, 3)
            rot90_score = 1 - np.sum(np.abs(data - rot90)) / (np.sum(np.abs(data)) + 1e-10)
            rot180_score = 1 - np.sum(np.abs(data - rot180)) / (np.sum(np.abs(data)) + 1e-10)
            rot270_score = 1 - np.sum(np.abs(data - rot270)) / (np.sum(np.abs(data)) + 1e-10)
            rot_score = max(rot90_score, rot180_score, rot270_score)
            results['rotational_symmetry'] = rot_score > 0.8
            results['symmetry_score'] += rot_score
        if len(data.shape) == 1 and len(data) >= 4:
            half_size = len(data) // 2
            downsampled = np.array([np.mean(data[i:i + 2]) for i in range(0, len(data), 2)])
            original_norm = (data[:half_size] - np.mean(data[:half_size])) / (np.std(data[:half_size]) + 1e-10)
            downsampled_norm = (downsampled - np.mean(downsampled)) / (np.std(downsampled) + 1e-10)
            if len(downsampled_norm) < len(original_norm):
                original_norm = original_norm[:len(downsampled_norm)]
            elif len(original_norm) < len(downsampled_norm):
                downsampled_norm = downsampled_norm[:len(original_norm)]
            scale_score = 1 - np.sum(np.abs(original_norm - downsampled_norm)) / (np.sum(np.abs(original_norm)) + 1e-10)
            results['scale_invariance'] = scale_score > 0.7
            results['symmetry_score'] += scale_score
        results['symmetry_score'] /= 3.0
        results['invariance_patterns'] = self._detect_patterns(data)
        return results
    def _detect_patterns(self, data):
        patterns = []
        if len(data.shape) == 1:
            n = len(data)
            for length in range(2, n // 2 + 1):
                for i in range(n - 2 * length + 1):
                    seq1 = data[i:i + length]
                    seq2 = data[i + length:i + 2 * length]
                    similarity = 1 - np.sum(np.abs(seq1 - seq2)) / (np.sum(np.abs(seq1)) + 1e-10)
                    if similarity > 0.9:
                        patterns.append({'type': 'repeating_sequence', 'start_index': i, 'length': length, 'similarity': float(similarity)})
        return patterns
    def _generate_sierpinski(self, iterations, symmetry=None):
        def sierpinski(points, iterations):
            if iterations == 0:
                plt.fill(*zip(*points), 'k')
                return
            p1, p2, p3 = points
            p12 = ((p1[0] + p2[0]) / 2, (p1[1] + p2[1]) / 2)
            p23 = ((p2[0] + p3[0]) / 2, (p2[1] + p3[1]) / 2)
            p31 = ((p3[0] + p1[0]) / 2, (p3[1] + p1[1]) / 2)
            sierpinski([p1, p12, p31], iterations - 1)
            sierpinski([p12, p2, p23], iterations - 1)
            sierpinski([p31, p23, p3], iterations - 1)
        points = [(0, 0), (1, 0), (0.5, np.sqrt(3) / 2)]
        if symmetry == 'reflection':
            points = [(0, 0), (1, 0), (0.5, -np.sqrt(3) / 2)]
        elif symmetry == 'rotation':
            theta = np.pi / 4
            points = [(np.cos(theta) * p[0] - np.sin(theta) * p[1], np.sin(theta) * p[0] + np.cos(theta) * p[1]) for p in points]
        sierpinski(points, iterations)
        plt.axis('equal')
        plt.axis('off')
    def _generate_mandelbrot(self, iterations, symmetry=None):
        size = 1000
        x = np.linspace(-2, 1, size)
        y = np.linspace(-1.5, 1.5, size)
        if symmetry == 'reflection':
            x = np.linspace(-1.5, 1.5, size)
            y = np.linspace(-1.5, 1.5, size)
        elif symmetry == 'rotation':
            x = np.linspace(-1.5, 1.5, size)
            y = np.linspace(-1.5, 1.5, size)
        X, Y = np.meshgrid(x, y)
        C = X + 1j * Y
        Z = np.zeros_like(C, dtype=complex)
        mask = np.zeros_like(C, dtype=bool)
        for i in range(iterations):
            Z = Z ** 2 + C
            new_mask = np.abs(Z) < 2
            mask = np.logical_and(mask, new_mask)
        plt.imshow(~mask.T, cmap='hot', extent=[-2, 1, -1.5, 1.5])
        plt.axis('off')
    def _generate_koch(self, iterations, symmetry=None):
        def koch_segment(p1, p2, iterations):
            if iterations == 0:
                plt.plot([p1[0], p2[0]], [p1[1], p2[1]], 'k')
                return
            dx = p2[0] - p1[0]
            dy = p2[1] - p1[1]
            p3 = (p1[0] + dx / 3, p1[1] + dy / 3)
            p5 = (p1[0] + 2 * dx / 3, p1[1] + 2 * dy / 3)
            angle = np.arctan2(dy, dx) + np.pi / 3
            dist = np.sqrt(dx ** 2 + dy ** 2) / 3
            p4 = (p3[0] + dist * np.cos(angle), p3[1] + dist * np.sin(angle))
            koch_segment(p1, p3, iterations - 1)
            koch_segment(p3, p4, iterations - 1)
            koch_segment(p4, p5, iterations - 1)
            koch_segment(p5, p2, iterations - 1)
        size = 1.0
        p1 = (0, 0)
        p2 = (size, 0)
        p3 = (size / 2, size * np.sqrt(3) / 2)
        if symmetry == 'reflection':
            p3 = (size / 2, -size * np.sqrt(3) / 2)
        elif symmetry == 'rotation':
            theta = np.pi / 4
            p1 = (np.cos(theta) * p1[0] - np.sin(theta) * p1[1], np.sin(theta) * p1[0] + np.cos(theta) * p1[1])
            p2 = (np.cos(theta) * p2[0] - np.sin(theta) * p2[1], np.sin(theta) * p2[0] + np.cos(theta) * p2[1])
            p3 = (np.cos(theta) * p3[0] - np.sin(theta) * p3[1], np.sin(theta) * p3[0] + np.cos(theta) * p3[1])
        koch_segment(p1, p2, iterations)
        koch_segment(p2, p3, iterations)
        koch_segment(p3, p1, iterations)
        plt.axis('equal')
        plt.axis('off')
    def _generate_recursive_squares(self, iterations, symmetry=None):
        def draw_square(x, y, size, angle, iterations):
            if iterations == 0:
                return
            corners = [(x - size / 2, y - size / 2), (x + size / 2, y - size / 2), (x + size / 2, y + size / 2), (x - size / 2, y + size / 2)]
            if angle != 0:
                corners = [(x + (px - x) * np.cos(angle) - (py - y) * np.sin(angle), y + (px - x) * np.sin(angle) + (py - y) * np.cos(angle)) for px, py in corners]
            plt.fill(*zip(*corners + [corners[0]]), 'k', alpha=0.7)
            new_size = size * 0.5
            new_iterations = iterations - 1
            for i, (px, py) in enumerate(corners):
                new_angle = angle
                if symmetry == 'rotation':
                    new_angle = angle + np.pi / 4
                elif symmetry == 'reflection' and i % 2 == 1:
                    new_angle = -angle
                draw_square(px, py, new_size, new_angle, new_iterations)
        draw_square(0.5, 0.5, 0.4, 0, iterations)
        plt.axis('equal')
        plt.axis('off')
        plt.xlim(0, 1)
        plt.ylim(0, 1)
if __name__ == '__main__':
    fractal = FractalInvariance()
    image_data = fractal.generate_invariant_pattern('sierpinski', 6)
    test_data = np.array([1, 2, 3, 4, 3, 2, 1])
    analysis = fractal.analyze_symmetry(test_data)
    print('Symmetry analysis:', analysis)