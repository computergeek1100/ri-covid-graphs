import csv
import os
from sys import platform
from selenium import webdriver
from selenium.webdriver.firefox.options import Options

DRIVER_PATH = "/usr/bin/geckodriver"

if platform == "darwin":
    DRIVER_PATH = "/usr/local/bin/geckodriver"

XPATH_PREFIX = "/html/body/app-bootstrap/ng2-bootstrap/bootstrap/div/div/div/div/div/div[1]/div[2]/div/div[1]/div/div[1]/div/lego-report/lego-canvas-container/div/file-drop-zone/span/content-section/"

options = Options()
options.headless = True

driver = webdriver.Firefox(
    options=options, executable_path=DRIVER_PATH, service_log_path=os.path.devnull)
driver.implicitly_wait(20)

driver.get(
    "https://datastudio.google.com/u/0/reporting/f95ea2dd-e77a-45dc-9735-f60e8581ff32/page/o9ivB")
date = driver.find_element_by_xpath(
    XPATH_PREFIX + "div[4]/canvas-component/div/div/div[1]/div/div/lego-table/div/div[3]/div/div[2]").text
dose1 = driver.find_element_by_xpath(
    XPATH_PREFIX + "div[3]/canvas-component/div/div/div[1]/div/div/lego-table/div/div[3]/div[1]/div[2]").text
dose2 = driver.find_element_by_xpath(
    XPATH_PREFIX + "div[3]/canvas-component/div/div/div[1]/div/div/lego-table/div/div[3]/div[2]/div[2]").text

with open("tmp.csv", mode="w") as tmpCSV:
    wrt = csv.writer(tmpCSV)
    wrt.writerow([date, dose1, dose2])

driver.quit()
