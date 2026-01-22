import asyncio
import sys
from typing import Optional
def format_bar(value: float, max_width: int=20) -> str:
    filled = int(value * max_width)
    return '█' * filled + ' ' * (max_width - filled)
def format_awareness_level(score: float) -> str:
    if score >= 0.9:
        return 'Highly Self-Aware'
    elif score >= 0.7:
        return 'Moderately Self-Aware'
    elif score >= 0.5:
        return 'Developing Self-Awareness'
    else:
        return 'Limited Self-Awareness'
async def show_status(soc):
    status = soc.get_autognosis_status()
    print('🧠 Autognosis - Hierarchical Self-Image Building System')
    print(f"Status: {('running' if status['running'] else 'stopped')}")
    print(f"Self-Image Levels: {status['max_levels']}")
    print(f"Total Insights Generated: {status['total_insights']}")
    print(f"Pending Optimizations: {status['pending_optimizations']}")
async def show_report(soc):
    status = soc.get_autognosis_status()
    print('🧠 Autognosis - Hierarchical Self-Image Building System')
    print(f"Status: {('running' if status['running'] else 'stopped')}")
    print(f"Self-Image Levels: {status['max_levels']}")
    print(f"Total Insights Generated: {status['total_insights']}")
    print(f"Pending Optimizations: {status['pending_optimizations']}")
    print()
    if soc.autognosis.current_self_images:
        print(f'Hierarchical Self-Images ({len(soc.autognosis.current_self_images)} levels):')
        for level, self_image in sorted(soc.autognosis.current_self_images.items()):
            print(f'  Level {level}: Confidence {self_image.confidence:.2f}, {len(self_image.meta_reflections)} reflections [{self_image.image_id}]')
        print()
    if soc.autognosis.current_insights:
        print('Recent Meta-Cognitive Insights:')
        for insight in soc.autognosis.current_insights[:5]:
            print(f'  • [{insight.insight_type}] {insight.description}')
        print()
    if soc.autognosis.current_self_images:
        highest_level = max(soc.autognosis.current_self_images.keys())
        self_image = soc.autognosis.current_self_images[highest_level]
        assessment = soc.autognosis.processor.get_self_awareness_assessment(self_image)
        print('Self-Awareness Assessment:')
        for metric, value in assessment.items():
            if metric != 'overall_score':
                bar = format_bar(value, max_width=17)
                metric_name = metric.replace('_', ' ').title()
                print(f'  {metric_name:25} {bar} {value:.3f}')
        overall = assessment['overall_score']
        print()
        print(f'Overall Self-Awareness Score: {overall:.3f} ({format_awareness_level(overall)})')
async def show_insights(soc):
    if not soc.autognosis.current_self_images:
        print('No self-images available yet. Run a cycle first.')
        return
    highest_level = max(soc.autognosis.current_self_images.keys())
    self_image = soc.autognosis.current_self_images[highest_level]
    assessment = soc.autognosis.processor.get_self_awareness_assessment(self_image)
    print('🧠 Self-Awareness Analysis')
    print()
    print(f'Analysis Level: {highest_level}')
    print(f'Confidence: {self_image.confidence:.2f}')
    print()
    print('Cognitive Dimensions:')
    for metric, value in assessment.items():
        if metric != 'overall_score':
            bar = format_bar(value, max_width=20)
            metric_name = metric.replace('_', ' ').title()
            print(f'  {metric_name:30} {bar} {value:.3f}')
    print()
    overall = assessment['overall_score']
    print(f'Overall Self-Awareness: {overall:.3f} ({format_awareness_level(overall)})')
    print()
    if self_image.meta_reflections:
        print('Meta-Reflections:')
        for reflection in self_image.meta_reflections:
            print(f'  • {reflection}')
        print()
    if self_image.behavioral_patterns:
        print('Detected Behavioral Patterns:')
        for pattern in self_image.behavioral_patterns:
            print(f'  • [{pattern.pattern_type}] {pattern.description}')
            print(f'    Confidence: {pattern.confidence:.2f}')
        print()
    if soc.autognosis.current_insights:
        print('Meta-Cognitive Insights:')
        for insight in soc.autognosis.current_insights:
            print(f'  • [{insight.insight_type}] {insight.description}')
            print(f'    Severity: {insight.severity}, Confidence: {insight.confidence:.2f}')
async def run_cli(args):
    from core import SelfOrganizingCore
    soc = SelfOrganizingCore()
    await soc.initialize()
    await soc.run_autognosis_cycle()
    try:
        if len(args) == 0 or args[0] != 'autognosis':
            print('Usage: orrrg autognosis [status|report|insights]')
            return 1
        command = args[1] if len(args) > 1 else 'status'
        if command == 'status' or command == '':
            await show_status(soc)
        elif command == 'report':
            await show_report(soc)
        elif command == 'insights':
            await show_insights(soc)
        else:
            print(f'Unknown command: {command}')
            print('Available commands: status, report, insights')
            return 1
        return 0
    finally:
        await soc.shutdown()
def main():
    args = sys.argv[1:]
    try:
        exit_code = asyncio.run(run_cli(args))
        sys.exit(exit_code)
    except KeyboardInterrupt:
        print('\nInterrupted by user')
        sys.exit(130)
    except Exception as e:
        print(f'Error: {e}', file=sys.stderr)
        sys.exit(1)
if __name__ == '__main__':
    main()