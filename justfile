test: 
   poetry run pytest .
   poetry run mypy .
   cat README.md | codedown python > readme.py
   poetry run python readme.py
   rm balances.json chart.json store.json

fix:    
   cat README.md | codedown python > readme.py
   isort . --float-to-top
   black .
   ruff format . # experimetal
   ruff check . --fix
   npx prettier README.md --write
