help:
	@echo "Please choose a target:"
	@echo "engine_data_create   Builds engine_data_create"
	@echo "data                 Builds data to test the engine"
.PHONY: help

engine_data_create:
	cd EngineDataCreate && lazbuild EngineDataCreate.lpi
.PHONY: engine_data_create

data: engine_data_create
	EngineDataCreate/bin/engine_data_create games/*.pgn > data/positions.data
.PHONY: data
