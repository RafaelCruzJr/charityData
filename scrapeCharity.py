from selenium.webdriver import Firefox 
from selenium.webdriver.common.by import By
from selenium.webdriver.firefox.options import Options
import pandas as pd
import time

def is_num(s):
	try:
		float(s)
		return True
	except ValueError:
		return False

url='https://www.charitywatch.org/charities?search=&category=all&letter=all&page='

options=Options()
options.add_argument('-headless')
driver=Firefox(options=options)
links=[]
nameList = ["empty"]

charList = []
counter=range(1,61)
for count in counter:
    driver.get(url+str(count))
    time.sleep(1)
    elements = driver.find_elements(By.CSS_SELECTOR, 'a')
    startRecording = False
    for e in elements:
        if startRecording:
            if is_num(e.text) or e.text == '‹':
                startRecording = False
            else:
                links.append(e.get_attribute('href'))
        if e.text == 'Z':
            startRecording = True
for n, link in enumerate(links):
    driver.get(link)
    time.sleep(1)
    name = driver.find_element(By.CSS_SELECTOR,"h1.mt-2").text[0:-5]
    if name not in nameList:
        nameList.append(name)
        try:
            grade = driver.find_element(By.CSS_SELECTOR,"div.charity-stat").text
            taxStatus = driver.find_element(By.CSS_SELECTOR,"section > div > div > div > div > p").text
            temp = driver.find_elements(By.CSS_SELECTOR,"h3 > span.text-nowrap > strong")
            programPerc = temp[0].text
            cost100 = temp[1].text
            temp = driver.find_elements(By.CSS_SELECTOR,"div.chart > div.row > div.col > h3.mb-2")
            temptemp = temp[0].text.replace(',','')
            totExpense = int(temptemp.replace('$',''))
            temptemp = temp[1].text.replace(',','')
            totContribution = int(temptemp.replace('$',''))
            govFunding = driver.find_element(By.CSS_SELECTOR,"div.chart > h3.mb-3").text
            temp = driver.find_elements(By.CSS_SELECTOR,'div.gandt > div.title')
            if len(temp) == 3:
                topRated = 1
                if "not" in temp[1].text:
                    governance = 0
                else:
                    governance = 1
                if "not" in temp[2].text:
                    transparency = 0
                else:
                    transparency = 1
            else:
                topRated = 0
                if "not" in temp[0].text:
                    governance = 0
                else:
                    governance = 1
                if "not" in temp[1].text:
                    transparency = 0
                else:
                    transparency = 1
            temp = driver.find_elements(By.CSS_SELECTOR,"div.col > table > tbody > tr > td[align='center']")
            tempList = []
            if len(temp) == 14:
                del temp[0:2]
            for item in temp:
                if "danger" in item.get_attribute("innerHTML"):
                    tempList.append(0)
                else:
                    tempList.append(1)
            temp = driver.find_elements(By.CSS_SELECTOR,"tr.border-bottom-0 > td[align='right']")
            salary = 0
            for item in temp:
                temptemp = item.text.replace(',','')
                salary += int(temptemp.replace('$',''))
            charList.append({
                "name":name,
                "grade":grade,
                "taxStatus":taxStatus,
                "programPerc":programPerc,
                "totExpense":totExpense,
                "cost100":cost100,
                "totContribution":totContribution,
                "govFunding":govFunding,
                "topRated":topRated,
                "governance":governance,
                "transparency":transparency,
                "transA":tempList[0],
                "transB":tempList[1],
                "govA":tempList[2],
                "govB":tempList[3],
                "govC":tempList[4],
                "govD":tempList[5],
                "govE":tempList[6],
                "govF":tempList[7],
                "govG":tempList[8],
                "govH":tempList[9],
                "govI":tempList[10],
                "privacy":tempList[11],
                "salary":salary,
                "nSalary":len(temp)
            })        
        except Exception:
            pass
driver.quit()
charData = pd.DataFrame(charList)
charData.to_csv("data.csv")



