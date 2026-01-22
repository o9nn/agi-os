import logging
from browser_interface import DeepTreeEchoBrowser
import time
logging.basicConfig(level=logging.INFO, format='%(asctime)s - %(name)s - %(levelname)s - %(message)s')
def test_human_like_interaction(browser):
    try:
        page = browser.get_or_create_page('Development')
        if not page:
            logging.error('Failed to get Development container page')
            return False
        logging.info('Testing search interaction...')
        page.goto('https://www.python.org')
        time.sleep(2)
        browser.human_like_interaction(page, 'click', selector='input[name="q"]')
        browser.human_like_interaction(page, 'type', text='machine learning')
        time.sleep(1)
        logging.info('Testing scroll behavior...')
        browser.human_like_interaction(page, 'scroll', amount=500, direction='down')
        time.sleep(2)
        browser.human_like_interaction(page, 'scroll', amount=200, direction='up')
        logging.info('Testing hover behavior...')
        browser.human_like_interaction(page, 'hover', selector='#container >> a[href="/about/"]', duration=1.5)
        return True
    except Exception as e:
        logging.error(f'Error in human-like interaction test: {str(e)}')
        return False
def main():
    browser = DeepTreeEchoBrowser()
    try:
        if not browser.init():
            logging.error('Failed to initialize browser')
            return
        if test_human_like_interaction(browser):
            logging.info('Successfully completed human-like interaction tests')
        else:
            logging.error('Failed to complete human-like interaction tests')
        input('Press Enter to close browser...')
    finally:
        browser.close()
if __name__ == '__main__':
    main()