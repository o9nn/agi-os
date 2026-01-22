import logging
from datetime import datetime, timedelta
from root.echo.user import get_projects, get_timelines, get_topics
logging.basicConfig(level=logging.INFO, format='%(asctime)s - %(name)s - %(levelname)s - %(message)s')
logger = logging.getLogger(__name__)
def test_projects():
    logger.info('Testing Projects component (spatial dimension)...')
    projects = get_projects()
    container_id = projects.create_container(name='Research Projects', description='Container for all research-related projects', tags=['research', 'academic'])
    category_id = projects.create_category(name='Cognitive Computing', parent_id=container_id, description='Projects related to cognitive computing research', tags=['ai', 'cognitive'])
    project_id = projects.create_project(name='Deep Tree Echo', category_id=category_id, description='Recursive computational thinking platform', status='active', priority='high', tags=['recursive', 'consciousness'])
    projects.add_project_resource(project_id=project_id, name='Architecture Diagram', resource_type='document', location='diagrams/architecture.svg', description='Visual representation of the DTE architecture')
    projects.add_project_resource(project_id=project_id, name='Research Paper', resource_type='document', location='papers/dte_recursive_thinking.pdf', description='Academic paper on recursive thinking')
    projects.update_project_progress(project_id, 0.75)
    project = projects.get_project(project_id)
    resources = projects.get_project_resources(project_id)
    logger.info(f"Project '{project['name']}' progress: {project['progress']:.0%}")
    logger.info(f'Project has {len(resources)} resources')
    state = projects.get_projects_state()
    logger.info(f'Projects system state: {state}')
    logger.info('Projects component tests completed successfully.')
    return True
def test_timelines():
    logger.info('Testing Timelines component (temporal dimension)...')
    timelines = get_timelines()
    timeline_id = timelines.create_timeline(name='DTE Development', timeline_type='project', description='Timeline for Deep Tree Echo development', tags=['development', 'planning'])
    phase1_id = timelines.create_phase(timeline_id=timeline_id, name='Planning Phase', start_date=datetime.now() - timedelta(days=30), end_date=datetime.now() - timedelta(days=15), phase_type='planning')
    phase2_id = timelines.create_phase(timeline_id=timeline_id, name='Development Phase', start_date=datetime.now() - timedelta(days=14), end_date=datetime.now() + timedelta(days=30), phase_type='development')
    timelines.add_event(timeline_id=timeline_id, title='Project Kickoff', timestamp=datetime.now() - timedelta(days=30), event_type='milestone', phase_id=phase1_id)
    timelines.add_event(timeline_id=timeline_id, title='Architecture Design', timestamp=datetime.now() - timedelta(days=25), duration=timedelta(days=5), phase_id=phase1_id)
    event3_id = timelines.add_event(timeline_id=timeline_id, title='User Level Implementation', timestamp=datetime.now() - timedelta(days=10), duration=timedelta(days=5), phase_id=phase2_id)
    timelines.add_reminder(event_id=event3_id, remind_at=datetime.now() + timedelta(days=1), description='Final code review for user level')
    events = timelines.get_events_in_timerange(start_time=datetime.now() - timedelta(days=15), end_time=datetime.now() + timedelta(days=15))
    logger.info(f"Timeline '{timelines.get_timeline(timeline_id)['name']}' has {len(events)} events in the last/next 15 days")
    upcoming = timelines.get_upcoming_events(days=30)
    logger.info(f'There are {len(upcoming)} upcoming events in the next 30 days')
    state = timelines.get_timelines_state()
    logger.info(f'Timelines system state: {state}')
    logger.info('Timelines component tests completed successfully.')
    return True
def test_topics():
    logger.info('Testing Topics component (causal dimension)...')
    topics = get_topics()
    forum_id = topics.create_forum(name='DTE Discussion', forum_type='discussion', description='Forum for discussing the Deep Tree Echo project', visibility='public', tags=['deep-tree-echo', 'discussion'])
    thread1_id = topics.create_thread(forum_id=forum_id, title='Architecture Design Patterns', content='What design patterns are most suitable for implementing recursive consciousness?', thread_type='discussion', tags=['architecture', 'design-patterns'])
    thread2_id = topics.create_thread(forum_id=forum_id, title='Optimizing Memory Systems', content='How can we optimize the different memory types in the system?', thread_type='question', tags=['memory', 'optimization'])
    message1_id = topics.add_message(thread_id=thread1_id, content='I think the Composite pattern would be ideal for representing recursive structures.')
    topics.add_message(thread_id=thread1_id, content='The Observer pattern could also be useful for implementing the consciousness stream.', parent_message_id=message1_id)
    message3_id = topics.add_message(thread_id=thread2_id, content='We should consider using different storage strategies for different memory types.')
    topics.add_reaction(message_id=message1_id, reaction_type='like')
    topics.add_reaction(message_id=message3_id, reaction_type='heart')
    topics.mark_as_answer(message3_id, True)
    thread1_messages = topics.get_thread_messages(thread1_id)
    thread2_messages = topics.get_thread_messages(thread2_id)
    logger.info(f"Thread '{topics.get_thread(thread1_id)['title']}' has {len(thread1_messages)} messages")
    logger.info(f"Thread '{topics.get_thread(thread2_id)['title']}' has {len(thread2_messages)} messages")
    reactions = topics.get_message_reactions(message1_id)
    logger.info(f'Message has {sum(reactions.values())} reactions')
    hierarchical_messages = topics.get_thread_messages(thread1_id, hierarchical=True)
    logger.info(f'Hierarchical view generated with {len(hierarchical_messages)} root messages')
    threads = topics.get_forum_threads(forum_id)
    logger.info(f"Forum '{topics.get_forum(forum_id)['name']}' has {len(threads)} threads")
    state = topics.get_topics_state()
    logger.info(f'Topics system state: {state}')
    logger.info('Topics component tests completed successfully.')
    return True
def main():
    logger.info('Starting user level component tests...')
    try:
        test_projects()
        test_timelines()
        test_topics()
        logger.info('All user level component tests completed successfully.')
    except Exception as e:
        logger.error(f'Error during testing: {e}')
        return False
    return True
if __name__ == '__main__':
    main()