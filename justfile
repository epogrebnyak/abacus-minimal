test: 
   poetry run pytest .
   poetry run mypy .
   poetry run python readme.py
   rm balances.json chart.json store.json


fix:    
   isort . --float-to-top
   black .
   ruff format . # experimetal
   ruff check . --fix
   npx prettier README.md --write
