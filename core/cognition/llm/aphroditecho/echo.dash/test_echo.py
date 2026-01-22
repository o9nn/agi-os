from selenium_interface import SeleniumInterface
import logging
def main():
    logging.basicConfig(level=logging.INFO)
    logger = logging.getLogger(__name__)
    interface = SeleniumInterface()
    try:
        if not interface.init():
            logger.error('Failed to initialize browser')
            return
        test_message = 'What are the key principles of recursive algorithms?'
        response, echo_analysis = interface.send_message(test_message)
        if response and echo_analysis:
            logger.info('Response received successfully')
            logger.info('Echo Analysis Results:')
            logger.info('Total Nodes: %s', echo_analysis['total_nodes'])
            logger.info('Average Echo: %.3f', echo_analysis['avg_echo'])
            logger.info('Maximum Echo: %.3f', echo_analysis['max_echo'])
            logger.info('Resonant Nodes: %s', echo_analysis['resonant_nodes'])
            logger.info('Tree Depth: %s', echo_analysis['depth'])
        else:
            logger.error('Failed to get response or analyze echoes')
    except (ImportError, AttributeError, KeyError, ValueError, RuntimeError) as e:
        logger.error('Error in test: %s', str(e))
    finally:
        pass
if __name__ == '__main__':
    main()