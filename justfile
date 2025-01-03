# Run pytest and mypy
test: 
   poetry run pytest .
   poetry run mypy abacus
   poetry run pyright abacus
   poetry run python examples/readme.py
   rm chart.json history.json

# Run linters
fix:    
   isort . --float-to-top
   black .
   ruff format . 
   ruff check . --fix

# Extract and run the code from README.md 
readme:
   npx prettier README.md --write
   echo \"\"\"This file is automatically generated from README.md\"\"\" > examples/readme.py
   cat README.md | npx codedown python >> examples/readme.py
   poetry run python examples/readme.py
   rm chart.json history.json
   isort examples/readme.py --float-to-top
   black examples/readme.py

# Run SQL examples
sql:
   sqlite3 < posts/1.sql
