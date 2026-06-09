import time
from selenium import webdriver
from selenium.webdriver.common.by import By
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC
from selenium.webdriver.support.ui import Select
import pandas as pd

# 1. URL da pagina inicial
url = 'https://data.usaswimming.org/datahub/recordssearch/recordprogressions'

print("Iniciando o navegador com Selenium...")

options = webdriver.ChromeOptions()
# options.add_argument('--headless') # Mantenha esta linha comentada para ver o que acontece
options.add_argument('--log-level=3') 
options.add_experimental_option('excludeSwitches', ['enable-logging'])

try:
    driver = webdriver.Chrome(options=options)
    driver.get(url)

    time.sleep(20)
    # 2. INTRODUZINDO ESPERAS EXP
    #    Cria um objeto de espera que aguardara ate 10 segundos.
    wait = WebDriverWait(driver, 20)
    print("Pagina carregada. Aguardando e preenchendo os filtros...")

    # 3. NOVOS SELETORES E ESPERAS
    #    Esperamos ate que cada menu <select> esteja presente na tela antes de interagir.
    #    Os seletores agora usam o atributo 'aria-label', que e mais estavel.
    
    try:
    # Filtro: Record List
        record_list_select = wait.until(EC.presence_of_element_located((By.ID, 'recordListId')))
        Select(record_list_select).select_by_value("201")

        # Filtro: Gender
        gender_select = wait.until(EC.presence_of_element_located((By.ID, 'competitionCategoryId')))
        Select(gender_select).select_by_visible_text('Male')

        # Filtro: Course
        course_select = wait.until(EC.presence_of_element_located((By.ID, 'courseId')))
        Select(course_select).select_by_visible_text('LCM')

        # Filtro: Event
        event_select = wait.until(EC.presence_of_element_located((By.ID, 'events')))
        Select(event_select).select_by_visible_text('100 FR LCM')
    except Exception as e:
        print(f"Ocorreu um erro ao tentar selecionar a opcao: {e}")
    print("Filtros preenchidos. Aguardando a tabela carregar...")

    # 4. Espera explicitamente pela tabela, usando o seletor de classe parcial
    wait.until(EC.presence_of_element_located((By.CLASS_NAME, '_UsasTable_xysrz_231')))
    
    html_da_pagina = driver.page_source
    
    print("HTML da pagina capturado. Lendo a tabela...")

    # 5. Usa o pandas para ler a tabela do HTML
    df_list = pd.read_html(html_da_pagina)
    
    tabela_correta = None
    for tabela in df_list:
        if 'Rank' in tabela.columns:
            tabela_correta = tabela
            break
            
    if tabela_correta is not None:
        print("\n--- DADOS EXTRAIDOS ---")
        print(tabela_correta)

        nome_arquivo = 'recordes_natacao_final.csv'
        tabela_correta.to_csv(nome_arquivo, index=False, encoding='utf-8-sig')
        
        print(f"\n>> Sucesso! Os dados foram salvos no arquivo '{nome_arquivo}'")
    else:
        print(">> Erro: Nao foi possivel encontrar a tabela de resultados na pagina.")

except Exception as e:
    print(f">> Ocorreu um erro inesperado com o Selenium: {e}")

finally:
    if 'driver' in locals():
        time.sleep(3) # Pausa rapida para ver o resultado final antes de fechar
        driver.quit()
        print("Navegador fechado.")