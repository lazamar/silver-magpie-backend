start:
	export $(cat .env | xargs) && stack exec silver-magpie
