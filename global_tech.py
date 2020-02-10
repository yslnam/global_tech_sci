from selenium import webdriver
import time
import re
import csv
import datetime

# Write CSV
csv_file = open('global_tech.csv', 'w', encoding='utf-8')
writer = csv.writer(csv_file)

writer.writerow(['title', 'speaker', 'num_views', 'summary', 'date', 'tags', 'num_translations', 'transcript', 'num_comments', 'video_url'])

# Initialize Chrome browser and global variables
driver = webdriver.Chrome()
p = 1

# Scrape
while True:
    try:
        driver.get(f"https://www.ted.com/talks?language=en&page={p}&sort=popular&topics%5B%5D=Global+issues&topics%5B%5D=Technology")
        p += 1
    except:
        break
    
    # Go to sleep zzzzz
    time.sleep(1)

    # Parse main video page
    main_url = 'https://www.ted.com' 
    titles = []
    speakers = []
    video_urls = []

    rows = driver.find_elements_by_xpath('//*[@class="media media--sm-v"]')

    for row in rows:
        titles.append(row.find_element_by_xpath('./div[@class="media__message"]//a[@class=" ga-link"]').text)
        speakers.append(row.find_element_by_xpath('./div[@class="media__message"]/h4[@class="h12 talk-link__speaker"]').text)
        video_urls.append(row.find_element_by_xpath('./div[@class="media__image media__image--thumb talk-link__image"]/a[@class=" ga-link"]').get_attribute('href'))

    # Grab title and speaker from each URL
    for title, speaker, video_url in zip(titles, speakers, video_urls):
        # print("="*50)
        # print(f'Title: {title}\nSpeaker: {speaker}\nURL: {video_url}')
        # print("="*50)

        # Get to individual video page
        driver.get(video_url)

        # Initialize empty dictionary for each field
        detail_dict = {}

        # Go to sleep zzzzz
        time.sleep(0.5)

        # Get details on page
        ## Number of views
        try:
            num_views = driver.find_element_by_xpath('//*[@id="content"]/div/div[4]/div[2]/section/div/div[2]/div/div[1]/span').text
            num_views = num_views.replace(',', '')
            # print("="*50)
            # print(f'Number of views: {num_views}')
        except:
            num_views = None
            print(f'Couldn\'t get number of views! {video_url}')

        ## Month and Year
        try:
            date = driver.find_element_by_xpath('.//div[@class=" f:.9 p-x:3@md c:black t-a:l "]/div[2]/div/span[2]').text
            date = date.replace('| ', '')
            date = datetime.datetime.strptime(date, '%B %Y').date()
            # print(f'Date: {date}')
        except:
            date = None
            print(f'Couldn\'t get date! {video_url}')

        ## Tags
        try:
            tags_button = driver.find_element_by_xpath('//*[@id="content"]/div/div[4]/div[2]/section/div/div[2]/div/div[3]/ul/li[4]/button')
            tags_button.click()
            tags = [x.text for x in driver.find_elements_by_xpath('//*[@id="content"]/div/div[4]/div[2]/section/div/div[2]/div/div[3]')]
            tags = tags[0].split('\n')[1:]
            tags.pop()
            # print(f'Tags: {tags}')
        except:
            tags = None
            print(f'Couldn\'t get tags! {video_url}')

        # Summary
        try:
            summary = driver.find_element_by_xpath('.//p[@class=" l-h:n m-b:1 "]').text
            # print(f'Summary: {summary}')
        except:
            summary = None
            print(f'Couldn\'t get summary! {video_url}')
            
        ## Number of translations
        try:
            num_translations = driver.find_element_by_xpath('//*[@id="content"]/div/div[4]/div[1]/div/a[2]/span[2]').text
            # Extract the number of foreign languages only
            num_translations = num_translations.replace(' languages', '')
            # print(f'Number of translations: {num_translations}')
        except:
            num_translations = None
            print(f'Couldn\'t get number of translations! {video_url}')

        ## Transcript
        try:
            script_button = driver.find_element_by_xpath('//span[contains(text(), "Transcript")]')
            script_button.click()
            time.sleep(1)
            transcript = " ".join([x.text for x in driver.find_elements_by_xpath('//a[contains(@class, "t-d:n") ]')])
            # print(f'Transcripts: {transcript}')
        except Exception as e:
            transcript = None
            print(e)
            print(f'Couldn\'t get transcript! {video_url}')

        ## Number of comments
        try:
            # num_comments = driver.find_element_by_xpath('//*[@id="content"]/div/div[4]/div[1]/div/a[4]/span[1]').text
            num_comments = driver.find_element_by_xpath('//span[contains(text(), "Comments")]').text
            num_comments = re.search('\d+', num_comments).group(0)
            # print(f'Number of comments: {num_comments}')
            # print("="*50)
        except:
            num_comments = None
            print(f'Couldn\'t get number of comments! {video_url}')
        
        # CSV fields
        detail_dict['title'] = title
        detail_dict['speaker'] = speaker
        detail_dict['num_views'] = num_views
        detail_dict['summary'] = summary
        detail_dict['date'] = date
        detail_dict['tags'] = tags
        detail_dict['num_translations'] = num_translations
        detail_dict['transcript'] = transcript
        detail_dict['num_comments'] = num_comments
        detail_dict['video_url'] = video_url
        
        # CSV values
        writer.writerow(detail_dict.values())

driver.close()