# ================

DATE=$(shell date "+%y-%m-%d")

TIME=$(shell date "+%H.%M")

APPLICATION_NAME=silver-magpie

COMMMIT_HASH=$(shell git rev-parse HEAD)

IMAGE=$(APPLICATION_NAME):$(COMMMIT_HASH)

# ================


start:
	stack exec silver-magpie

watch:
	stack build --file-watch --pedantic

# Will create and start the server running on port 80
# Will show the output on terminal as well as save it to a file
deploy:
	make docker-deploy 2>&1 | tee _logs/$(DATE)--$(TIME)--$(COMMMIT_HASH).log

# ============== PRIVATE TARGETS =====================
# Rules below are not public

docker-deploy:
	make docker-build
	make docker-run
	make docker-cleanup
	@echo "------------------------."
	@echo "Deployment successful."
	@echo "------------------------."

docker-build:
	docker build \
		--tag $(IMAGE) \
		.

docker-run:
	docker rm -f $(shell docker ps -q) ; \
	docker run \
		-p 8081:8080 \
		-v $(shell pwd)/_env:/home/app/_env \
		-d \
		$(IMAGE)

# Will remove all silver-magpie docker images that are not being executed.
docker-cleanup:
	docker images \
		| grep -G $(APPLICATION_NAME) \
		| awk '{print $3}' \
		| xargs docker rmi -f \
		; exit 0
