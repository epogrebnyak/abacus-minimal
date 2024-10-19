test: 
   poetry run pytest .
   poetry run mypy .
   just readme

fix:    
   cat README.md | codedown python > readme.py
   isort . --float-to-top
   black .
   ruff format . # experimetal
   ruff check . --fix
   npx prettier README.md --write

readme:
   cat README.md | codedown python > readme.py
   poetry run python readme.py
   rm balances.json chart.json store.json
