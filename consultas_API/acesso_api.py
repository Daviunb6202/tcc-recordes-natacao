import os 
import requests
import pandas as pd

headers = { 
    "User-Agent": "Mozilla/5.0",
    "Accept": "application/json",
    "Origin": "https://www.worldaquatics.com", 
    "Referer": "https://www.worldaquatics.com"
}

all_comps = []
page = 0

print("Buscando lista de competicoes...")

while True:
    try:
        date_start = "1970-01-01T00:00:00+00:00" 
        date_end = "2026-01-01T00:00:00+00:00"
        
        url = "https://api.worldaquatics.com/fina/competitions"
        params = {
            "pageSize": 100,
            "venueDateFrom": date_start, 
            "venueDateTo": date_end,    
            "disciplines":  "SW",       
            "group": "FINA",
            "sort": "dateFrom,asc",     
            "page": page 
        }
        
        res = requests.get(url, params=params, headers=headers)
        res.raise_for_status()
        data = res.json()
        
        comps = data.get("content", [])
        all_comps.extend(comps)
        
        print(f"Pagina {page} processada. Total competicoes: {len(all_comps)}")
        
        if page >= data.get("pageInfo", {}).get("numPages", 0) - 1: 
            break
        
        page += 1

    except Exception as e:
        print(f"Erro na paginacao de competicoes: {e}")
        break

ids = []
dicionario_comp = {}

# Filtragem de categorias
for c in all_comps:
    comp_name = c.get("name", "").lower()
    
    is_swimming = "SW" in c.get("disciplines", [])
    
    is_world_champ = "world" in comp_name and "championships" in comp_name

    is_not_cup = "cup" not in comp_name
    is_not_base_or_masters = "junior" not in comp_name and "masters" not in comp_name

    is_long_course = "25m" not in comp_name and "short course" not in comp_name

    if is_swimming and is_world_champ and is_not_cup and is_not_base_or_masters and is_long_course:
        c_id = c["id"]
        dicionario_comp[c_id] = { 
            "Competition": c["name"],
            "City": c.get("location", {}).get("city", "Unknown")
        }
        ids.append(c_id)

print(f"Encontradas {len(ids)} competicoes relevantes.")

final_rows = []

# Processamento dos Eventos
if ids:
    for i, competition_id in enumerate(ids):
        print(f"Processando competicao {i+1}/{len(ids)}: {dicionario_comp[competition_id]['Competition']}")
        
        try:
            events_url = f"https://api.worldaquatics.com/fina/competitions/{competition_id}/events"
            res_events = requests.get(events_url, headers=headers)
            res_events.raise_for_status()
            events_data = res_events.json()
        except Exception as e:
            print(f"Erro ao baixar eventos da comp {competition_id}: {e}")
            continue

        sw_comps = []
        dicionario_discipline = {}

        # Identificar IDs dos 100m Livre
        for sport in events_data.get("Sports", []):
            if sport.get("Code") == "SW":
                for d in sport.get("DisciplineList", []):
                    d_name = d.get("DisciplineName", "").strip()
                    d_name_lower = d_name.lower()
                    
                    # Filtro mais flexivel (pega "Men 100m Freestyle" ou "Men's 100m Freestyle")
                    if "100m freestyle" in d_name_lower:
                        if "men" in d_name_lower or "women" in d_name_lower:
                            # Ignora revezamentos se houver
                            if "relay" not in d_name_lower:
                                d_id = d.get("Id")
                                sw_comps.append(d_id)
                                dicionario_discipline[d_id] = {
                                    "id": competition_id,
                                    "Gender": d.get("Gender")
                                }

        # Baixar resultados de cada evento
        for event_id in sw_comps:
            try:
                event_url = f"https://api.worldaquatics.com/fina/events/{event_id}"
                res_event = requests.get(event_url, headers=headers)
                res_event.raise_for_status()
                event_results = res_event.json()
            except Exception as e:
                print(f"Erro evento {event_id}: {e}")
                continue

            # Iterar Heats e buscar 'final' de forma flexivel
            found_final = False
            for heat in event_results.get('Heats', []):
                phase_name = heat.get('PhaseName', '').lower()
                
                if 'final' in phase_name and 'semifinal' not in phase_name:
                    
                    final_date = heat.get('Date')
                    results_list = heat.get('Results', [])
                    
                    if not results_list:
                        continue

                    found_final = True
                    rows = []
                    for athlete in results_list:
                        if not athlete: continue
                        
                        full_name = f"{athlete.get('FirstName', '')} {athlete.get('LastName', '')}".strip()
                        
                        row_dict = {
                            "Competition": dicionario_comp[competition_id]["Competition"],
                            "Location": dicionario_comp[competition_id]["City"],
                            "Date": final_date,
                            "Gender": dicionario_discipline[event_id]["Gender"],
                            "Team": athlete.get("NAT", "N/A"),
                            "Athlete": full_name,
                            "Results": athlete.get("Time", ""),
                            "Rank": athlete.get("Rank", "") 
                        }
                        rows.append(row_dict)
                    

                    final_rows.extend(rows)
                    break 

# Geracao do DataFrame
if final_rows:
    df = pd.DataFrame(final_rows)
    df['Date'] = pd.to_datetime(df['Date'], errors='coerce')
    df_final = df.drop_duplicates(subset=['Competition', 'Gender', 'Athlete'])
    df_final = df_final.sort_values(by=['Date', 'Competition'])

    base_dir = os.path.dirname(os.path.abspath(__file__))
    output_dir = os.path.join(base_dir, "output")
    os.makedirs(output_dir, exist_ok=True)

    excel_path = os.path.join(output_dir, "Resultados_Finais_WorldAquatics.xlsx")
    df_final.to_excel(excel_path, index=False)

    print(f"\nExcel salvo com sucesso: {excel_path}")
    print(df_final.head())
else:
    print("\nNenhum resultado encontrado. Verifique os filtros de data/nome.")
