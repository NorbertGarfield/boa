use crate::{object::JsObject, Context, JsString, JsValue};

use rustc_hash::FxHashMap;

#[test]
fn best_avail_loc() {
    let no_extensions_locale = JsString::new("en-US");
    let available_locales = Vec::<JsString>::new();
    assert_eq!(
        crate::builtins::intl::best_available_locale(&available_locales, &no_extensions_locale,),
        None
    );

    let no_extensions_locale = JsString::new("de-DE");
    let available_locales = vec![no_extensions_locale.clone()];
    assert_eq!(
        crate::builtins::intl::best_available_locale(&available_locales, &no_extensions_locale,),
        Some(no_extensions_locale)
    );

    let locale_part = "fr".to_string();
    let no_extensions_locale = JsString::new(locale_part.clone() + &"-CA".to_string());
    let available_locales = vec![JsString::new(locale_part.clone())];
    assert_eq!(
        crate::builtins::intl::best_available_locale(&available_locales, &no_extensions_locale,),
        Some(JsString::new(locale_part))
    );

    let ja_kana_t = JsString::new("ja-Kana-JP-t");
    let ja_kana = JsString::new("ja-Kana-JP");
    let no_extensions_locale = JsString::new("ja-Kana-JP-t-it-latn-it");
    let available_locales = vec![ja_kana_t.clone(), ja_kana.clone()];
    assert_eq!(
        crate::builtins::intl::best_available_locale(&available_locales, &no_extensions_locale,),
        Some(ja_kana)
    );
}

#[test]
fn lookup_match() {
    // available: [], requested: []
    let available_locales = Vec::<JsString>::new();
    let requested_locales = Vec::<JsString>::new();

    let matcher = crate::builtins::intl::lookup_matcher(&available_locales, &requested_locales);
    assert_eq!(matcher.locale, crate::builtins::intl::default_locale());
    assert_eq!(matcher.extension, "");

    // available: [de-DE], requested: []
    let available_locales = vec![JsString::new("de-DE")];
    let requested_locales = Vec::<JsString>::new();

    let matcher = crate::builtins::intl::lookup_matcher(&available_locales, &requested_locales);
    assert_eq!(matcher.locale, crate::builtins::intl::default_locale());
    assert_eq!(matcher.extension, "");

    // available: [fr-FR], requested: [fr-FR-u-hc-h12]
    let available_locales = vec![JsString::new("fr-FR")];
    let requested_locales = vec![JsString::new("fr-FR-u-hc-h12")];

    let matcher = crate::builtins::intl::lookup_matcher(&available_locales, &requested_locales);
    assert_eq!(matcher.locale, "fr-FR");
    assert_eq!(matcher.extension, "-u-hc-h12");

    // available: [es-ES], requested: [es-ES]
    let available_locales = vec![JsString::new("es-ES")];
    let requested_locales = vec![JsString::new("es-ES")];

    let matcher = crate::builtins::intl::best_fit_matcher(&available_locales, &requested_locales);
    assert_eq!(matcher.locale, "es-ES");
    assert_eq!(matcher.extension, "");
}

#[test]
fn insert_unicode_ext() {
    let locale = JsString::new("hu-HU");
    let ext = JsString::empty();
    assert_eq!(
        crate::builtins::intl::insert_unicode_extension_and_canonicalize(&locale, &ext),
        locale
    );

    let locale = JsString::new("hu-HU");
    let ext = JsString::new("-u-hc-h12");
    assert_eq!(
        crate::builtins::intl::insert_unicode_extension_and_canonicalize(&locale, &ext),
        JsString::new("hu-HU-u-hc-h12")
    );

    let locale = JsString::new("hu-HU-x-PRIVATE");
    let ext = JsString::new("-u-hc-h12");
    assert_eq!(
        crate::builtins::intl::insert_unicode_extension_and_canonicalize(&locale, &ext),
        JsString::new("hu-HU-u-hc-h12-x-PRIVATE")
    );
}

#[test]
fn uni_ext_comp() {
    let ext = JsString::new("-u-ca-japanese-hc-h12");
    let components = crate::builtins::intl::unicode_extension_components(&ext);
    assert_eq!(components.attributes.is_empty(), true);
    assert_eq!(components.keywords.len(), 2);
    assert_eq!(components.keywords[0].key, "ca");
    assert_eq!(components.keywords[0].value, "japanese");
    assert_eq!(components.keywords[1].key, "hc");
    assert_eq!(components.keywords[1].value, "h12");

    let ext = JsString::new("-u-alias-co-phonebk-ka-shifted");
    let components = crate::builtins::intl::unicode_extension_components(&ext);
    assert_eq!(components.attributes, vec![JsString::new("alias")]);
    assert_eq!(components.keywords.len(), 2);
    assert_eq!(components.keywords[0].key, "co");
    assert_eq!(components.keywords[0].value, "phonebk");
    assert_eq!(components.keywords[1].key, "ka");
    assert_eq!(components.keywords[1].value, "shifted");

    let ext = JsString::new("-u-ca-buddhist-kk-nu-thai");
    let components = crate::builtins::intl::unicode_extension_components(&ext);
    assert_eq!(components.attributes.is_empty(), true);
    assert_eq!(components.keywords.len(), 3);
    assert_eq!(components.keywords[0].key, "ca");
    assert_eq!(components.keywords[0].value, "buddhist");
    assert_eq!(components.keywords[1].key, "kk");
    assert_eq!(components.keywords[1].value, "");
    assert_eq!(components.keywords[2].key, "nu");
    assert_eq!(components.keywords[2].value, "thai");

    let ext = JsString::new("-u-ca-islamic-civil");
    let components = crate::builtins::intl::unicode_extension_components(&ext);
    assert_eq!(components.attributes.is_empty(), true);
    assert_eq!(components.keywords.len(), 1);
    assert_eq!(components.keywords[0].key, "ca");
    assert_eq!(components.keywords[0].value, "islamic-civil");
}

#[test]
fn locale_resolution() {
    let mut context = Context::default();

    // test lookup
    let available_locales = Vec::<JsString>::new();
    let requested_locales = Vec::<JsString>::new();
    let relevant_extension_keys = Vec::<JsString>::new();
    let locale_data = FxHashMap::default();
    let options = crate::builtins::intl::DateTimeFormatRecord {
        locale_matcher: JsString::new("lookup"),
        properties: FxHashMap::default(),
    };

    let locale_record = crate::builtins::intl::resolve_locale(
        &available_locales,
        &requested_locales,
        &options,
        &relevant_extension_keys,
        &locale_data,
        &mut context,
    );
    assert_eq!(
        locale_record.locale,
        crate::builtins::intl::default_locale()
    );
    assert_eq!(
        locale_record.data_locale,
        crate::builtins::intl::default_locale()
    );
    assert_eq!(locale_record.properties.is_empty(), true);

    // test best fit
    let available_locales = Vec::<JsString>::new();
    let requested_locales = Vec::<JsString>::new();
    let relevant_extension_keys = Vec::<JsString>::new();
    let locale_data = FxHashMap::default();
    let options = crate::builtins::intl::DateTimeFormatRecord {
        locale_matcher: JsString::new("best-fit"),
        properties: FxHashMap::default(),
    };

    let locale_record = crate::builtins::intl::resolve_locale(
        &available_locales,
        &requested_locales,
        &options,
        &relevant_extension_keys,
        &locale_data,
        &mut context,
    );
    assert_eq!(
        locale_record.locale,
        crate::builtins::intl::default_locale()
    );
    assert_eq!(
        locale_record.data_locale,
        crate::builtins::intl::default_locale()
    );
    assert_eq!(locale_record.properties.is_empty(), true);

    // available: [es-ES], requested: [es-ES]
    let available_locales = vec![JsString::new("es-ES")];
    let requested_locales = vec![JsString::new("es-ES")];
    let relevant_extension_keys = Vec::<JsString>::new();
    let locale_data = FxHashMap::default();
    let options = crate::builtins::intl::DateTimeFormatRecord {
        locale_matcher: JsString::new("lookup"),
        properties: FxHashMap::default(),
    };

    let locale_record = crate::builtins::intl::resolve_locale(
        &available_locales,
        &requested_locales,
        &options,
        &relevant_extension_keys,
        &locale_data,
        &mut context,
    );
    assert_eq!(locale_record.locale, "es-ES");
    assert_eq!(locale_record.data_locale, "es-ES");
    assert_eq!(locale_record.properties.is_empty(), true);

    // available: [zh-CN], requested: []
    let available_locales = vec![JsString::new("zh-CN")];
    let requested_locales = Vec::<JsString>::new();
    let relevant_extension_keys = Vec::<JsString>::new();
    let locale_data = FxHashMap::default();
    let options = crate::builtins::intl::DateTimeFormatRecord {
        locale_matcher: JsString::new("lookup"),
        properties: FxHashMap::default(),
    };

    let locale_record = crate::builtins::intl::resolve_locale(
        &available_locales,
        &requested_locales,
        &options,
        &relevant_extension_keys,
        &locale_data,
        &mut context,
    );
    assert_eq!(
        locale_record.locale,
        crate::builtins::intl::default_locale()
    );
    assert_eq!(
        locale_record.data_locale,
        crate::builtins::intl::default_locale()
    );
    assert_eq!(locale_record.properties.is_empty(), true);
}

#[test]
fn get_opt() {
    let mut context = Context::default();

    let values = Vec::<JsValue>::new();
    let options_obj = JsValue::undefined();
    let get_option_result = crate::builtins::intl::get_option(
        &options_obj,
        "",
        "",
        &values,
        &JsValue::String(JsString::empty()),
        &mut context,
    );
    assert_eq!(get_option_result.is_err(), true);

    let values = Vec::<JsValue>::new();
    let fallback = JsValue::String(JsString::new("fallback"));
    let options_obj = JsValue::new(JsObject::empty());
    let get_option_result =
        crate::builtins::intl::get_option(&options_obj, "", "", &values, &fallback, &mut context)
            .expect("GetOption should not fail on fallback test");
    assert_eq!(get_option_result, fallback);

    let values = Vec::<JsValue>::new();
    let fallback = JsValue::String(JsString::new("fallback"));
    let options_obj = JsObject::empty();
    let locale_value = JsValue::String(JsString::new("en-US"));
    options_obj
        .set("Locale", locale_value.clone(), true, &mut context)
        .expect("Setting a property should not fail");
    let get_option_result = crate::builtins::intl::get_option(
        &JsValue::new(options_obj),
        "Locale",
        "number",
        &values,
        &fallback,
        &mut context,
    );
    assert_eq!(get_option_result.is_err(), true);

    let values = Vec::<JsValue>::new();
    let fallback = JsValue::String(JsString::new("fallback"));
    let options_obj = JsObject::empty();
    let locale_value = JsValue::String(JsString::new("en-US"));
    options_obj
        .set("Locale", locale_value.clone(), true, &mut context)
        .expect("Setting a property should not fail");
    let get_option_result = crate::builtins::intl::get_option(
        &JsValue::new(options_obj),
        "Locale",
        "string",
        &values,
        &fallback,
        &mut context,
    )
    .expect("GetOption should not fail on string test");
    assert_eq!(get_option_result, locale_value);

    let fallback = JsValue::String(JsString::new("fallback"));
    let options_obj = JsObject::empty();
    let locale_value = JsValue::String(JsString::new("en-US"));
    let values = vec![locale_value.clone()];
    options_obj
        .set("Locale", locale_value.clone(), true, &mut context)
        .expect("Setting a property should not fail");
    let get_option_result = crate::builtins::intl::get_option(
        &JsValue::new(options_obj),
        "Locale",
        "string",
        &values,
        &fallback,
        &mut context,
    )
    .expect("GetOption should not fail on values test");
    assert_eq!(get_option_result, locale_value);

    let fallback = JsValue::String(JsString::new("fallback"));
    let options_obj = JsObject::empty();
    let locale_value = JsValue::String(JsString::new("en-US"));
    let other_locale_value = JsValue::String(JsString::new("de-DE"));
    let values = vec![other_locale_value];
    options_obj
        .set("Locale", locale_value.clone(), true, &mut context)
        .expect("Setting a property should not fail");
    let get_option_result = crate::builtins::intl::get_option(
        &JsValue::new(options_obj),
        "Locale",
        "string",
        &values,
        &fallback,
        &mut context,
    );
    assert_eq!(get_option_result.is_err(), true);
}

#[test]
fn to_date_time_opts() {
    let mut context = Context::default();

    let options_obj = JsObject::empty();
    options_obj
        .set("timeStyle", JsObject::empty(), true, &mut context)
        .expect("Setting a property should not fail");
    let date_time_opts = crate::builtins::intl::date_time_format::to_date_time_options(
        &JsValue::new(options_obj),
        "date",
        "date",
        &mut context,
    );
    assert_eq!(date_time_opts.is_err(), true);

    let options_obj = JsObject::empty();
    options_obj
        .set("dateStyle", JsObject::empty(), true, &mut context)
        .expect("Setting a property should not fail");
    let date_time_opts = crate::builtins::intl::date_time_format::to_date_time_options(
        &JsValue::new(options_obj),
        "time",
        "time",
        &mut context,
    );
    assert_eq!(date_time_opts.is_err(), true);

    let date_time_opts = crate::builtins::intl::date_time_format::to_date_time_options(
        &JsValue::undefined(),
        "date",
        "date",
        &mut context,
    )
    .expect("toDateTimeOptions should not fail in date test");

    let numeric_jsstring = JsValue::String(JsString::new("numeric"));
    assert_eq!(
        date_time_opts.get("year", &mut context),
        Ok(numeric_jsstring.clone())
    );
    assert_eq!(
        date_time_opts.get("month", &mut context),
        Ok(numeric_jsstring.clone())
    );
    assert_eq!(
        date_time_opts.get("day", &mut context),
        Ok(numeric_jsstring.clone())
    );

    let date_time_opts = crate::builtins::intl::date_time_format::to_date_time_options(
        &JsValue::undefined(),
        "time",
        "time",
        &mut context,
    )
    .expect("toDateTimeOptions should not fail in time test");

    let numeric_jsstring = JsValue::String(JsString::new("numeric"));
    assert_eq!(
        date_time_opts.get("hour", &mut context),
        Ok(numeric_jsstring.clone())
    );
    assert_eq!(
        date_time_opts.get("minute", &mut context),
        Ok(numeric_jsstring.clone())
    );
    assert_eq!(
        date_time_opts.get("second", &mut context),
        Ok(numeric_jsstring.clone())
    );

    let date_time_opts = crate::builtins::intl::date_time_format::to_date_time_options(
        &JsValue::undefined(),
        "any",
        "all",
        &mut context,
    )
    .expect("toDateTimeOptions should not fail when testing required = 'any'");

    let numeric_jsstring = JsValue::String(JsString::new("numeric"));
    assert_eq!(
        date_time_opts.get("year", &mut context),
        Ok(numeric_jsstring.clone())
    );
    assert_eq!(
        date_time_opts.get("month", &mut context),
        Ok(numeric_jsstring.clone())
    );
    assert_eq!(
        date_time_opts.get("day", &mut context),
        Ok(numeric_jsstring.clone())
    );
    assert_eq!(
        date_time_opts.get("hour", &mut context),
        Ok(numeric_jsstring.clone())
    );
    assert_eq!(
        date_time_opts.get("minute", &mut context),
        Ok(numeric_jsstring.clone())
    );
    assert_eq!(
        date_time_opts.get("second", &mut context),
        Ok(numeric_jsstring.clone())
    );
}

#[test]
fn nonterminals() {
    let mut context = Context::default();

    let nonterminal_calendar_options = vec![
        JsValue::String(JsString::new("")),
        JsValue::String(JsString::new("a")),
        JsValue::String(JsString::new("ab")),
        JsValue::String(JsString::new("abcdefghi")),
        JsValue::String(JsString::new("abc-abcdefghi")),
        JsValue::String(JsString::new("!invalid!")),
        JsValue::String(JsString::new("-gregory-")),
        JsValue::String(JsString::new("gregory-")),
        JsValue::String(JsString::new("gregory--")),
        JsValue::String(JsString::new("gregory-nu")),
        JsValue::String(JsString::new("gregory-nu-")),
        JsValue::String(JsString::new("gregory-nu-latn")),
        JsValue::String(JsString::new("gregoryé")),
        JsValue::String(JsString::new("gregory역법")),
    ];

    for calendar_opt in nonterminal_calendar_options {
        assert_eq!(
            crate::builtins::intl::date_time_format::is_nonterminal(&calendar_opt, &mut context),
            false
        );
    }

    let terminal_calendar_options = vec![
        JsValue::String(JsString::new("buddhist")),
        JsValue::String(JsString::new("chinese")),
        JsValue::String(JsString::new("coptic")),
        JsValue::String(JsString::new("dangi")),
        JsValue::String(JsString::new("ethioaa")),
        JsValue::String(JsString::new("ethiopic")),
        JsValue::String(JsString::new("gregory")),
        JsValue::String(JsString::new("hebrew")),
        JsValue::String(JsString::new("indian")),
        JsValue::String(JsString::new("islamic")),
        JsValue::String(JsString::new("islamic-umalqura")),
        JsValue::String(JsString::new("islamic-tbla")),
        JsValue::String(JsString::new("islamic-civil")),
        JsValue::String(JsString::new("islamic-rgsa")),
        JsValue::String(JsString::new("iso8601")),
        JsValue::String(JsString::new("japanese")),
        JsValue::String(JsString::new("persian")),
        JsValue::String(JsString::new("roc")),
    ];

    for calendar_opt in terminal_calendar_options {
        assert_eq!(
            crate::builtins::intl::date_time_format::is_nonterminal(&calendar_opt, &mut context),
            true
        );
    }
}

#[test]
fn build_date_time_fmt() {
    let mut context = Context::default();

    let date_time_fmt_obj = crate::builtins::intl::date_time_format::DateTimeFormat::constructor(
        &JsValue::undefined(),
        &Vec::<JsValue>::new(),
        &mut context,
    );
    assert_eq!(date_time_fmt_obj.is_err(), false);
}
