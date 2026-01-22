import logging
import numpy as np
import matplotlib.pyplot as plt
from matplotlib.colors import LinearSegmentedColormap
from deep_tree_echo import DeepTreeEcho
from emotional_dynamics import CoreEmotion
logging.basicConfig(level=logging.INFO, format='%(asctime)s - %(name)s - %(levelname)s - %(message)s')
logger = logging.getLogger(__name__)
def create_test_tree():
    dte = DeepTreeEcho(echo_threshold=0.5, max_depth=5, use_julia=True)
    root = dte.create_tree("Let's explore and discover the curious world of playful learning.")
    fear_node = dte.add_child(root, "I'm afraid of the terrifying consequences if we don't act with caution.")
    care_node = dte.add_child(root, 'We must nurture and protect those who need our help and love.')
    anger_node = dte.add_child(root, 'The rage builds inside as we see destruction and hate around us.')
    dte.add_child(fear_node, 'This scary feeling paralyzes me with dread and anxiety.')
    dte.add_child(care_node, 'Compassionately caring for others brings joy and connection.')
    dte.add_child(anger_node, 'Channeling this furious energy toward positive change.')
    logger.info('Created test tree with emotional content')
    return dte
def show_emotional_states(dte):
    [e.name for e in CoreEmotion]
    nodes = []
    queue = [dte.root]
    while queue:
        node = queue.pop(0)
        nodes.append(node)
        queue.extend(node.children)
    print('\nEmotional States:')
    print('-' * 80)
    print(f"{'Node Content':<40} | {'Dominant Emotion':<15} | {'Compound':<20} | {'Echo Value':<10}")
    print('-' * 80)
    for node in nodes:
        content = node.content[:37] + '...' if len(node.content) > 40 else node.content
        dominant = node.metadata.get('dominant_emotion', 'N/A')
        compound = node.metadata.get('compound_emotion', 'N/A')
        echo = f'{node.echo_value:.3f}'
        print(f'{content:<40} | {dominant:<15} | {compound:<20} | {echo:<10}')
def visualize_emotions(dte):
    colors = ['gold', 'red', 'purple', 'pink', 'green', 'gray', 'cyan']
    emotion_cmap = LinearSegmentedColormap.from_list('emotions', colors, N=7)
    nodes = []
    labels = []
    emotional_states = []
    queue = [dte.root]
    while queue:
        node = queue.pop(0)
        nodes.append(node)
        content = node.content[:20] + '...' if len(node.content) > 20 else node.content
        labels.append(content)
        emotional_states.append(node.emotional_state)
        queue.extend(node.children)
    emotional_states = np.array(emotional_states)
    fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(15, 8))
    im = ax1.imshow(emotional_states, cmap=emotion_cmap, aspect='auto')
    ax1.set_yticks(np.arange(len(labels)))
    ax1.set_yticklabels(labels)
    ax1.set_xticks(np.arange(7))
    ax1.set_xticklabels([e.name for e in CoreEmotion], rotation=45)
    ax1.set_title('Emotional State Distribution')
    cbar = fig.colorbar(im, ax=ax1)
    cbar.set_label('Intensity')
    echo_values = [node.echo_value for node in nodes]
    dominant_indices = np.argmax(emotional_states, axis=1)
    bar_colors = [colors[idx] for idx in dominant_indices]
    ax2.barh(np.arange(len(echo_values)), echo_values, color=bar_colors)
    ax2.set_yticks(np.arange(len(labels)))
    ax2.set_yticklabels(labels)
    ax2.set_xlabel('Echo Value')
    ax2.set_title('Echo Values by Node')
    ax2.set_xlim(0, 1)
    plt.tight_layout()
    plt.savefig('emotional_deep_tree_visualization.png')
    logger.info('Saved visualization to emotional_deep_tree_visualization.png')
def main():
    logger.info('Starting emotional dynamics demonstration')
    dte = create_test_tree()
    logger.info('Simulating emotional dynamics')
    dte.propagate_emotions()
    show_emotional_states(dte)
    visualize_emotions(dte)
    emotion_analysis = dte.analyze_emotional_patterns()
    print('\nEmotional Pattern Analysis:')
    print('-' * 80)
    print('Emotion Distribution:')
    for i, emotion in enumerate(CoreEmotion):
        print(f"  {emotion.name}: {emotion_analysis['emotion_distribution'][i]:.3f}")
    print('\nDominant Emotions:')
    for emotion, count in emotion_analysis['dominant_emotions'].items():
        print(f'  {emotion}: {count}')
    print('\nCompound Emotions:')
    for emotion, count in emotion_analysis['compound_emotions'].items():
        print(f'  {emotion}: {count}')
    print(f"\nEmotional Complexity: {emotion_analysis['emotional_complexity']:.3f}")
    print(f"Emotional Intensity: {emotion_analysis['emotional_intensity']:.3f}")
    logger.info('Demonstration completed')
if __name__ == '__main__':
    main()