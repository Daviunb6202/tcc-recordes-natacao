import time
import os
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

os.makedirs('output_ow', exist_ok=True)

try:
    driver = webdriver.Chrome(options=options)
    driver.get(url)

    time.sleep(20)
    wait = WebDriverWait(driver, 20)
    print("Pagina carregada. Configurando filtros fixos...")

    try:
        record_list_select = wait.until(EC.presence_of_element_located((By.ID, 'recordListId')))
        Select(record_list_select).select_by_value("201")
        
        generos = ['Male', 'Female']
        
        for genero in generos:
            print(f"\n=======================================")
            print(f"=== INICIANDO BUSCA PARA: {genero} ===")
            print(f"=======================================")

            gender_select = wait.until(EC.presence_of_element_located((By.ID, 'competitionCategoryId')))
            Select(gender_select).select_by_visible_text(genero)

            print(f"[{genero}] Selecionando Course (LCM)...")
            course_dropdown = wait.until(EC.element_to_be_clickable((By.ID, 'courseId')))
            course_dropdown.click()
            time.sleep(1)
            course_option = wait.until(EC.element_to_be_clickable((By.XPATH, "//*[text()='LCM']")))
            course_option.click()

            print(f"[{genero}] Selecionando Event (100 FR LCM)...")
            event_dropdown = wait.until(EC.element_to_be_clickable((By.CSS_SELECTOR, '#root > div:nth-child(3) > div > div.row.usas-extra-bottom-margin > div > div:nth-child(4) > div._DisplayComponent_1c35t_495 > form > div > div:nth-child(4) > div > div:nth-child(2) > button')))
            event_dropdown.click()
            time.sleep(1) 
            event_option = wait.until(EC.element_to_be_clickable((By.XPATH, "//*[text()='100 FR LCM']")))
            event_option.click()

            print(f"[{genero}] Filtros preenchidos. Aguardando a tabela atualizar...")
            time.sleep(5)

            wait.until(EC.presence_of_element_located((By.XPATH, '//*[@id="root"]/div[2]/div/div[3]/div/div[2]/div[4]/div/div[1]/table')))
            
            html_da_pagina = driver.page_source
            print(f"[{genero}] HTML da pagina capturado. Lendo a tabela...")

            df_list = pd.read_html(html_da_pagina)
            
            tabela_correta = None
            for tabela in df_list:
                if 'Rank' in tabela.columns:
                    tabela_correta = tabela
                    break
                    
            if tabela_correta is not None:
                nome_arquivo = f'output_ow/recordes_natacao_{genero}.csv'
                tabela_correta.to_csv(nome_arquivo, index=False, encoding='utf-8-sig')
                
                print(f"[{genero}] >> Sucesso! Os dados foram salvos no arquivo '{nome_arquivo}'")
            else:
                print(f"[{genero}] >> Erro: Nao foi possivel encontrar a tabela de resultados na pagina.")
            
            time.sleep(3)

    except Exception as e:
        print(f"Ocorreu um erro ao tentar selecionar as opcoes: {e}")

except Exception as e:
    print(f">> Ocorreu um erro inesperado com o Selenium: {e}")

finally:
    if 'driver' in locals():
        time.sleep(3) 
        driver.quit()
        print("\nNavegador fechado. Processo concluido.")