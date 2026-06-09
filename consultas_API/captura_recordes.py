import time
from selenium import webdriver
from selenium.webdriver.common.by import By
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC
from selenium.webdriver.support.ui import Select
import pandas as pd

url = 'https://data.usaswimming.org/datahub/recordssearch/recordprogressions'

print("Iniciando o navegador com Selenium...")

options = webdriver.ChromeOptions()
options.add_argument('--log-level=3') 
options.add_experimental_option('excludeSwitches', ['enable-logging'])

try:
    driver = webdriver.Chrome(options=options)
    driver.get(url)

    time.sleep(20)
    wait = WebDriverWait(driver, 20)
    print("Pagina carregada. Aguardando e preenchendo os filtros...")

    try:
        record_list_select = wait.until(EC.presence_of_element_located((By.ID, 'recordListId')))
        Select(record_list_select).select_by_value("201")

        gender_select = wait.until(EC.presence_of_element_located((By.ID, 'competitionCategoryId')))
        Select(gender_select).select_by_visible_text('Male')

        course_select = wait.until(EC.presence_of_element_located((By.ID, 'courseId')))
        Select(course_select).select_by_visible_text('LCM')

        event_select = wait.until(EC.presence_of_element_located((By.XPATH, '//*[@id="root"]/div[2]/div/div[3]/div/div[2]/div[1]/form/div/div[4]/div/div[1]')))
        time.sleep(2)
        Select(event_select).select_by_visible_text('100 FR LCM')
    except Exception as e:
        print(f"Ocorreu um erro ao tentar selecionar a opcao: {e}")
    print("Filtros preenchidos. Aguardando a tabela carregar...")

    wait.until(EC.presence_of_element_located((By.XPATH, '//*[@id="root"]/div[2]/div/div[3]/div/div[2]/div[4]/div/div[1]/table')))
    
    html_da_pagina = driver.page_source
    
    print("HTML da pagina capturado. Lendo a tabela...")

    df_list = pd.read_html(html_da_pagina)
    
    tabela_correta = None
    for tabela in df_list:
        if 'Rank' in tabela.columns:
            tabela_correta = tabela
            break
            
    if tabela_correta is not None:
        print("\n--- DADOS EXTRAIDOS ---")
        print(tabela_correta)

        nome_arquivo = 'output_ow/recordes_natacao_final.csv'
        tabela_correta.to_csv(nome_arquivo, index=False, encoding='utf-8-sig')
        
        print(f"\n>> Sucesso! Os dados foram salvos no arquivo '{nome_arquivo}'")
    else:
        print(">> Erro: Nao foi possivel encontrar a tabela de resultados na pagina.")

except Exception as e:
    print(f">> Ocorreu um erro inesperado com o Selenium: {e}")

finally:
    if 'driver' in locals():
        time.sleep(3) 
        driver.quit()
        print("Navegador fechado.")
