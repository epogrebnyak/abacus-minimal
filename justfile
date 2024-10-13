test: 
   pytest .
   mypy .
   python readme.py
   rm balances.json chart.json store.json


fix:    
   isort . --float-to-top
   black .
   ruff check . --fix
   npx prettier README.md --write

