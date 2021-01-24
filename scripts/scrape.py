from selenium import webdriver
from selenium.webdriver.firefox.options import Options

DRIVER_PATH = "/usr/local/bin/geckodriver"

options = Options()
options.headless = True
driver = webdriver.Firefox(
    options=options, executable_path=DRIVER_PATH)
driver.implicitly_wait(20)
driver.get(
    "https://datastudio.google.com/u/0/reporting/f95ea2dd-e77a-45dc-9735-f60e8581ff32/page/o9ivB")
dose1 = driver.find_element_by_xpath(
    "/html/body/app-bootstrap/ng2-bootstrap/bootstrap/div/div/div/div/div/div[1]/div[2]/div/div[1]/div/div[1]/div/lego-report/lego-canvas-container/div/file-drop-zone/span/content-section/div[3]/canvas-component/div/div/div[1]/div/div/lego-table/div/div[3]/div[1]/div[2]").text
print(dose1)
driver.quit()
