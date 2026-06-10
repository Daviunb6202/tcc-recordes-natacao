Nesta pasta consultas_API são apresentados os seguintes arquivos: 

acesso_api : Acessa API da World Aquatics a fim de obter os resultados das finais de cada competição World Aquatics Championships e Fina World Championships, e faz o "parsing" do JSON associado. 

tratamento_dados : Uso da biblioteca pandas para padronização dos valores dos resultados em segundos, em formato decimal, dado que ao longo do tempo as performances foram cronometradas de diversas formas. Além disso, pontua-se também a padronização dos tempos em torno da média e desvio padrão do bloco de competição.

captura_recordes : Web Scrapping da página USA Swimming, para pegar a tabela de recordes masculino e feminimo dos 100 M livres em piscina longa.

tratamento_recores : Uso dda biblioteca pandas para remover e padronizar o nome de algumas colunas.

Na pasta output estão salvos exemplos dos arquivos csv utilizados neste processo de coleta dos dados empíricos do estudo.
