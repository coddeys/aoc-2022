.PHONE=ghcid
ghcid:
	ghcid -c "stack ghci" \
	  --reload=app/Main.hs \
	  --reload=src/Lib.hs \
          --test Main.main \
	  --restart=./package.yaml
