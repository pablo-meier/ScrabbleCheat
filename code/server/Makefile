
BINGAD_OUT=./lib/bingad/bin/gaddag_parser
DICT_PATH=priv/dicts

shell: dictionaries
	rebar3 shell

release: dictionaries
	rebar3 release

dictionaries: twl06 sowpods zynga

TWL06_PATH=$(DICT_PATH)/twl06
twl06: bingad
	[ -e priv/twl06.dict ] || ( $(BINGAD_OUT) $(TWL06_PATH).txt && mv $(TWL06_PATH).dict priv/ )

SOWPODS_PATH=$(DICT_PATH)/sowpods
sowpods: bingad
	[ -e priv/sowpods.dict ] || ( $(BINGAD_OUT) $(SOWPODS_PATH).txt && mv $(SOWPODS_PATH).dict priv/ )

ZYNGA_PATH=$(DICT_PATH)/zynga
zynga: bingad
	[ -e priv/zynga.dict ] || ( $(BINGAD_OUT) $(ZYNGA_PATH).txt && mv $(ZYNGA_PATH).dict priv/ )

bingad:
	[ -e $(BINGAD_OUT) ] || ( cd lib/bingad && $(MAKE) )

clean:
	cd lib/bingad && $(MAKE) clean
	rebar3 clean
	rm -rf priv/twl06.dict priv/sowpods.dict priv/zynga.dict _build log
