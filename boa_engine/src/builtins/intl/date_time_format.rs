//! This module implements the global `Intl.DateTimeFormat` object.
//!
//! `Intl.DateTimeFormat` is a built-in object that has properties and methods for date and time i18n.
//!
//! More information:
//!  - [ECMAScript reference][spec]
//!
//! [spec]: https://tc39.es/ecma402/#datetimeformat-objects

use crate::{
    builtins::intl::{
        default_locale, get_number_option, get_option, resolve_locale, DateTimeFormatRecord,
        LocaleDataRecord,
    },
    builtins::{Array, Intl, JsArgs},
    context::intrinsics::StandardConstructors,
    object::{
        internal_methods::get_prototype_from_constructor, ConstructorBuilder, JsFunction, JsObject,
        ObjectData,
    },
    property::PropertyNameKind,
    Context, JsResult, JsString, JsValue,
};

use boa_gc::{Finalize, Trace};
use boa_profiler::Profiler;
use rustc_hash::FxHashMap;
use std::cmp::max;
use std::cmp::min;

/// JavaScript `Intl.DateTimeFormat` object.
#[derive(Debug, Clone, Trace, Finalize)]
pub struct DateTimeFormat {
    initialized_date_time_format: bool,
    locale: JsString,
    calendar: JsValue,
    numbering_system: JsValue,
    time_zone: JsValue,
    weekday: JsValue,
    era: JsValue,
    year: JsValue,
    month: JsValue,
    day: JsValue,
    day_period: JsValue,
    hour: JsValue,
    minute: JsValue,
    second: JsValue,
    fractional_second_digits: JsValue,
    time_zone_name: JsValue,
    hour_cycle: JsValue,
    pattern: JsValue,
    range_patterns: JsValue,
    bound_format: JsString,
    date_style: JsValue,
    time_style: JsValue,
}

impl DateTimeFormat {
    const NAME: &'static str = "DateTimeFormat";

    pub(super) fn init(context: &mut Context) -> JsFunction {
        let _timer = Profiler::global().start_event(Self::NAME, "init");

        ConstructorBuilder::new(context, Self::constructor)
            .name(Self::NAME)
            .length(0)
            .build()
    }
}

impl DateTimeFormat {
    /// The `Intl.DateTimeFormat` constructor is the `%DateTimeFormat%` intrinsic object and a standard built-in property of the `Intl` object.
    ///
    /// More information:
    ///  - [ECMAScript reference][spec]
    ///  - [MDN documentation][mdn]
    ///
    /// [spec]: https://tc39.es/ecma402/#datetimeformat-objects
    /// [mdn]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Intl/DateTimeFormat
    pub(crate) fn constructor(
        new_target: &JsValue,
        args: &[JsValue],
        context: &mut Context,
    ) -> JsResult<JsValue> {
        // 1. If NewTarget is undefined, let newTarget be the active function object, else let newTarget be NewTarget.
        let prototype = get_prototype_from_constructor(
            new_target,
            StandardConstructors::date_time_format,
            context,
        )?;
        // 2. Let dateTimeFormat be ? OrdinaryCreateFromConstructor(newTarget, "%DateTimeFormat.prototype%",
        // « [[InitializedDateTimeFormat]], [[Locale]], [[Calendar]], [[NumberingSystem]], [[TimeZone]], [[Weekday]],
        // [[Era]], [[Year]], [[Month]], [[Day]], [[DayPeriod]], [[Hour]], [[Minute]], [[Second]],
        // [[FractionalSecondDigits]], [[TimeZoneName]], [[HourCycle]], [[Pattern]], [[BoundFormat]] »).
        let date_time_format = JsObject::from_proto_and_data(
            prototype,
            ObjectData::date_time_format(Box::new(Self {
                initialized_date_time_format: true,
                locale: JsString::from("en-US"),
                calendar: JsValue::String(JsString::from("gregory")),
                numbering_system: JsValue::String(JsString::from("arab")),
                time_zone: JsValue::String(JsString::from("UTC")),
                weekday: JsValue::String(JsString::from("narrow")),
                era: JsValue::String(JsString::from("narrow")),
                year: JsValue::String(JsString::from("numeric")),
                month: JsValue::String(JsString::from("narrow")),
                day: JsValue::String(JsString::from("numeric")),
                day_period: JsValue::String(JsString::from("narrow")),
                hour: JsValue::String(JsString::from("numeric")),
                minute: JsValue::String(JsString::from("numeric")),
                second: JsValue::String(JsString::from("numeric")),
                fractional_second_digits: JsValue::new(1.0),
                time_zone_name: JsValue::String(JsString::from("")),
                hour_cycle: JsValue::String(JsString::from("h24")),
                pattern: JsValue::String(JsString::from("{hour}:{minute}")),
                range_patterns: JsValue::undefined(),
                bound_format: JsString::from("undefined"),
                date_style: JsValue::String(JsString::from("full")),
                time_style: JsValue::String(JsString::from("full")),
            })),
        );

        // 3. Perform ? InitializeDateTimeFormat(dateTimeFormat, locales, options).
        let maybe_locales = args.get_or_undefined(0);
        let maybe_options = args.get_or_undefined(1);
        let maybe_date_time_format =
            initialize_date_time_format(&date_time_format, maybe_locales, maybe_options, context);

        let date_time_format = match maybe_date_time_format {
            Ok(date_time_fmt) => date_time_fmt,
            Err(date_time_err) => return Err(date_time_err),
        };

        // TODO 4. If the implementation supports the normative optional constructor mode of 4.3 Note 1, then
        // TODO a. Let this be the this value.
        // TODO b. Return ? ChainDateTimeFormat(dateTimeFormat, NewTarget, this).

        // 5. Return dateTimeFormat.
        Ok(date_time_format.into())
    }
}

/// The abstract operation `toDateTimeOptions` is called with arguments `options`, `required` and
/// `defaults`.
///
/// More information:
///  - [ECMAScript reference][spec]
///
/// [spec]: https://tc39.es/ecma402/#sec-todatetimeoptions
pub fn to_date_time_options(
    options: &JsValue,
    required: &str,
    defaults: &str,
    context: &mut Context,
) -> JsResult<JsObject> {
    // 1. If options is undefined, let options be null;
    // otherwise let options be ? ToObject(options).
    let maybe_options = if options.is_undefined() {
        Ok(JsObject::empty())
    } else {
        options.to_object(context)
    };
    let options = maybe_options.unwrap_or_else(|_| JsObject::empty());

    // 2. Let options be ! OrdinaryObjectCreate(options).
    let options = JsObject::from_proto_and_data(options, ObjectData::ordinary());

    // 3. Let needDefaults be true.
    let mut need_defaults = true;

    // 4. If required is "date" or "any", then
    if required.eq("date") || required.eq("any") {
        // a. For each property name prop of « "weekday", "year", "month", "day" », do
        let property_names = vec!["weekday", "year", "month", "day"];
        // i. Let value be ? Get(options, prop).
        // ii. If value is not undefined, let needDefaults be false.
        need_defaults = property_names.iter().all(|prop_name| {
            options
                .get(*prop_name, context)
                .unwrap_or_else(|_| JsValue::undefined())
                .is_undefined()
        });
    }

    // 5. If required is "time" or "any", then
    if required.eq("time") || required.eq("any") {
        // a. For each property name prop of « "dayPeriod", "hour", "minute", "second",
        // "fractionalSecondDigits" », do
        let property_names = vec![
            "dayPeriod",
            "hour",
            "minute",
            "second",
            "fractionalSecondDigits",
        ];
        // i. Let value be ? Get(options, prop).
        // ii. If value is not undefined, let needDefaults be false.
        need_defaults = property_names.iter().all(|prop_name| {
            options
                .get(*prop_name, context)
                .unwrap_or_else(|_| JsValue::undefined())
                .is_undefined()
        });
    }

    // 6. Let dateStyle be ? Get(options, "dateStyle").
    let date_style = options
        .get("dateStyle", context)
        .unwrap_or_else(|_| JsValue::undefined());

    // 7. Let timeStyle be ? Get(options, "timeStyle").
    let time_style = options
        .get("timeStyle", context)
        .unwrap_or_else(|_| JsValue::undefined());

    // 8. If dateStyle is not undefined or timeStyle is not undefined, let needDefaults be false.
    if !date_style.is_undefined() || !time_style.is_undefined() {
        need_defaults = false;
    }

    // 9. If required is "date" and timeStyle is not undefined, then
    if required.eq("date") && !time_style.is_undefined() {
        // a. Throw a TypeError exception.
        return context.throw_type_error("'date' is required, but timeStyle was defined");
    }

    // 10. If required is "time" and dateStyle is not undefined, then
    if required.eq("time") && !date_style.is_undefined() {
        // a. Throw a TypeError exception.
        return context.throw_type_error("'time' is required, but dateStyle was defined");
    }

    // 11. If needDefaults is true and defaults is either "date" or "all", then
    if need_defaults && (defaults.eq("date") || defaults.eq("all")) {
        // a. For each property name prop of « "year", "month", "day" », do
        let property_names = vec!["year", "month", "day"];
        // i. Perform ? CreateDataPropertyOrThrow(options, prop, "numeric").
        for prop_name in property_names {
            options
                .create_data_property_or_throw(prop_name, "numeric", context)
                .expect("CreateDataPropertyOrThrow must not fail");
        }
    }

    // 12. If needDefaults is true and defaults is either "time" or "all", then
    if need_defaults && (defaults.eq("time") || defaults.eq("all")) {
        // a. For each property name prop of « "hour", "minute", "second" », do
        let property_names = vec!["hour", "minute", "second"];
        // i. Perform ? CreateDataPropertyOrThrow(options, prop, "numeric").
        for prop_name in property_names {
            options
                .create_data_property_or_throw(prop_name, "numeric", context)
                .expect("CreateDataPropertyOrThrow must not fail");
        }
    }

    // 13. Return options.
    Ok(options)
}

/// The abstract operation `is_nonterminal` determines whether `opt` `JsValue` contains a
/// nonterminal symbol.
///
/// More information:
///  - [Unicode LDML reference][spec]
///
/// [spec]: https://www.unicode.org/reports/tr35/#Unicode_locale_identifier
pub(crate) fn is_nonterminal(opt: &JsValue, context: &mut Context) -> bool {
    let opt_str = opt
        .to_string(context)
        .unwrap_or_else(|_| JsString::empty())
        .to_string();
    if opt_str.is_empty() {
        return false;
    }

    // nonterminal = alphanum{3,8} (sep alphanum{3,8})*
    // Any number of alphanumeric characters between 3 and 8,
    // separated by dash,
    // followed by any number of alphanumeric characters between 3 and 8 (repeated)

    // First, replace all underscores (legacy format) with dashes.
    let opt_str = opt_str.replace('_', "-");

    // Next, split the string by dashes.
    let options_vec: Vec<&str> = opt_str.split('-').collect();

    // If the vector contains less than 1 element, that cannot be a nonterminal.
    if options_vec.is_empty() {
        return false;
    }

    // Check that each slice is has length between 3 and 8 and all characters are alphanumeric.
    for option in options_vec {
        if option.len() < 3 || option.len() > 8 {
            return false;
        }

        if option.chars().any(|character| !character.is_alphanumeric()) {
            return false;
        }
    }

    true
}

/// The value of the `LocaleData` internal slot is implementation-defined within the following
/// constraints, for all locale values locale:
/// [[`LocaleData`]].[[<locale>]].[[nu]] must be a List that does not include the values
/// "native", "traditio", or "finance".
/// [[`LocaleData`]].[[<locale>]].[[hc]] must be « null, "h11", "h12", "h23", "h24" ».
/// [[`LocaleData`]].[[<locale>]].[[hourCycle]] must be a String value equal to "h11", "h12", "h23",
/// or "h24".
///
/// More information:
///  - [ECMAScript reference][spec]
///
/// [spec]: https://tc39.es/ecma402/#sec-intl.datetimeformat-internal-slots
fn build_locale_data(context: &mut Context) -> LocaleDataRecord {
    let mut locale_data_entry = FxHashMap::default();
    let nu_values = vec![JsValue::String(JsString::new("arab"))];
    locale_data_entry.insert(
        JsString::new("nu"),
        JsValue::Object(Array::create_array_from_list(
            nu_values.into_iter().map(Into::into),
            context,
        )),
    );
    let hc_values = vec![
        JsValue::String(JsString::new("h11")),
        JsValue::String(JsString::new("h12")),
        JsValue::String(JsString::new("h23")),
        JsValue::String(JsString::new("h24")),
    ];
    locale_data_entry.insert(
        JsString::new("hc"),
        JsValue::Object(Array::create_array_from_list(
            hc_values.into_iter().map(Into::into),
            context,
        )),
    );
    locale_data_entry.insert(
        JsString::new("hourCycle"),
        JsValue::String(JsString::new("h24")),
    );

    let ca_values = vec![JsValue::String(JsString::new("gregory"))];
    locale_data_entry.insert(
        JsString::new("ca"),
        JsValue::Object(Array::create_array_from_list(
            ca_values.into_iter().map(Into::into),
            context,
        )),
    );

    let format_values = vec![JsValue::Object(JsObject::empty())];
    let calendar_formats = JsObject::empty();
    calendar_formats
        .set(
            JsString::new("gregory"),
            JsValue::Object(Array::create_array_from_list(
                format_values.into_iter().map(Into::into),
                context,
            )),
            true,
            context,
        )
        .expect("Failed to set calendar formats");
    locale_data_entry.insert(JsString::new("formats"), JsValue::Object(calendar_formats));

    let mut locale_data = FxHashMap::default();
    let default_locale_str = default_locale();
    locale_data.insert(default_locale_str, locale_data_entry);

    locale_data
}

/// The value of the `RelevantExtensionKeys` internal slot is « "ca", "hc", "nu" ».
///
/// More information:
///  - [ECMAScript reference][spec]
///
/// [spec]: https://tc39.es/ecma402/#sec-intl.datetimeformat-internal-slots
fn build_relevant_ext_keys() -> Vec<JsString> {
    vec![
        JsString::new("ca"),
        JsString::new("hc"),
        JsString::new("nu"),
    ]
}

/// `AvailableLocales` is a `List` that contains structurally valid and canonicalized Unicode
/// BCP 47 locale identifiers identifying the locales for which the implementation provides the
/// functionality of the constructed objects. Language tags on the list must not have a Unicode
/// locale extension sequence. The list must include the value returned by the `DefaultLocale`
/// abstract operation, and must not include duplicates. Implementations must include in
/// `AvailableLocales` locales that can serve as fallbacks in the algorithm used to resolve locales.
///
/// More information:
///  - [ECMAScript reference][spec]
///
/// [spec]: https://tc39.es/ecma402/#sec-internal-slots
fn build_available_locales() -> Vec<JsString> {
    let default_locale_str = default_locale();
    let canonicalized_locale = default_locale_str.replace('_', "-");
    let splitted_locale: Vec<&str> = canonicalized_locale.split('-').collect();
    let default_locale_fallback = splitted_locale
        .get(0)
        .expect("Failed to split default locale");
    let available_locales = vec![default_locale_str, JsString::new(default_locale_fallback)];

    available_locales
}

/// `FormatOptionsRecord` type aggregates `hour_cycle` string and `properties` map.
#[derive(Debug)]
struct FormatOptionsRecord {
    hour_cycle: JsValue,
    properties: FxHashMap<JsString, JsValue>,
}

/// `DateTimeComponents` type contains `internal_slot` string `property` map and list of `values`
#[derive(Debug)]
struct DateTimeComponents {
    internal_slot: JsString,
    property: JsString,
    values: Vec<JsValue>,
}

/// The `DateTimeStyleFormat` abstract operation accepts arguments `dateStyle` and `timeStyle`,
/// which are each either undefined, "full", "long", "medium", or "short", at least one of which
/// is not undefined, and `styles`, which is a record from
/// %`DateTimeFormat`%.[[`LocaleData`]].[[<locale>]].[[styles]].[[<calendar>]] for some locale `locale`
/// and calendar `calendar`. It returns the appropriate format record for date time formatting
/// based on the parameters.
///
/// More information:
///  - [ECMAScript reference][spec]
///
/// [spec]: https://tc39.es/ecma402/#sec-date-time-style-format
fn date_time_style_format(
    date_style: &JsValue,
    time_style: &JsValue,
    styles: &JsObject,
    context: &mut Context,
) -> JsResult<JsValue> {
    // FIXME untested
    // 1. If timeStyle is not undefined, then
    let time_format = if time_style.is_undefined() {
        JsValue::undefined()
    } else {
        // a. Assert: timeStyle is one of "full", "long", "medium", or "short".
        let available_time_styles = vec![
            JsValue::String(JsString::new("full")),
            JsValue::String(JsString::new("long")),
            JsValue::String(JsString::new("medium")),
            JsValue::String(JsString::new("short")),
        ];

        if !available_time_styles
            .iter()
            .any(|style| style.eq(time_style))
        {
            return context.throw_type_error("DateTimeStyleFormat: unsupported time style");
        }

        // b. Let timeFormat be styles.[[TimeFormat]].[[<timeStyle>]].
        let time_format_styles = styles
            .get("TimeFormat", context)
            .expect("Failed to get TimeFormat");
        let time_format_styles = time_format_styles
            .to_object(context)
            .expect("Failed to cast TimeFormat to object");
        let time_style_str = time_style
            .to_string(context)
            .unwrap_or_else(|_| JsString::empty());
        let time_style_str = time_style_str.to_string();
        time_format_styles
            .get(time_style_str, context)
            .unwrap_or_else(|_| JsValue::undefined())
    };

    // 2. If dateStyle is not undefined, then
    let date_format = if date_style.is_undefined() {
        JsValue::undefined()
    } else {
        // a. Assert: dateStyle is one of "full", "long", "medium", or "short".
        let available_date_styles = vec![
            JsValue::String(JsString::new("full")),
            JsValue::String(JsString::new("long")),
            JsValue::String(JsString::new("medium")),
            JsValue::String(JsString::new("short")),
        ];

        if !available_date_styles
            .iter()
            .any(|style| style.eq(date_style))
        {
            return context.throw_type_error("DateTimeStyleFormat: unsupported date style");
        }

        // b. Let dateFormat be styles.[[DateFormat]].[[<dateStyle>]].
        let date_format_styles = styles
            .get("DateFormat", context)
            .expect("Failed to get DateFormat");
        let date_format_styles = date_format_styles
            .to_object(context)
            .expect("Failed to cast DateFormat to object");
        let date_style_str = date_style
            .to_string(context)
            .unwrap_or_else(|_| JsString::empty());
        let date_style_str = date_style_str.to_string();
        date_format_styles
            .get(date_style_str, context)
            .unwrap_or_else(|_| JsValue::undefined())
    };

    // 3. If dateStyle is not undefined and timeStyle is not undefined, then
    if !date_style.is_undefined() && !time_style.is_undefined() {
        // a. Let format be a new Record.
        let format = JsObject::empty();

        // b. Add to format all fields from dateFormat except [[pattern]] and [[rangePatterns]].
        let date_format_obj = date_format
            .to_object(context)
            .expect("Failed to cast dateFormat to object");
        let entries_list = date_format_obj
            .enumerable_own_property_names(PropertyNameKind::KeyAndValue, context)?;

        for entry in entries_list {
            let entry_obj = entry.to_object(context)?;
            let entry_key = entry_obj.get(0, context)?;
            let entry_key_str = entry_key.to_string(context)?;
            if entry_key_str.ne(&JsString::new("pattern"))
                && entry_key_str.ne(&JsString::new("rangePatterns"))
            {
                let entry_val = entry_obj.get(1, context)?;
                format.set(entry_key_str, entry_val, true, context)?;
            }
        }

        // c. Add to format all fields from timeFormat except
        // [[pattern]], [[rangePatterns]], [[pattern12]], and [[rangePatterns12]], if present.
        let time_format_obj = time_format
            .to_object(context)
            .expect("Failed to cast timeFormat to object");
        let entries_list = time_format_obj
            .enumerable_own_property_names(PropertyNameKind::KeyAndValue, context)?;
        for entry in entries_list {
            let entry_obj = entry.to_object(context)?;
            let entry_key = entry_obj.get(0, context)?;
            let entry_key_str = entry_key.to_string(context)?;
            if entry_key_str.ne(&JsString::new("pattern"))
                && entry_key_str.ne(&JsString::new("rangePatterns"))
                && entry_key_str.ne(&JsString::new("pattern12"))
                && entry_key_str.ne(&JsString::new("rangePatterns12"))
            {
                let entry_val = entry_obj.get(1, context)?;
                format.set(entry_key_str, entry_val, true, context)?;
            }
        }

        // d. Let connector be styles.[[DateTimeFormat]].[[<dateStyle>]].
        let date_time_format = styles.get("DateTimeFormat", context)?;
        let date_time_format = date_time_format.to_object(context)?;
        let date_style_str = date_style
            .to_string(context)
            .unwrap_or_else(|_| JsString::empty());
        let date_style_str = date_style_str.to_string();
        let connector = date_time_format.get(date_style_str, context)?;
        let connector_str = connector.to_string(context)?;

        // e. Let pattern be the string connector with the substring "{0}" replaced with
        // timeFormat.[[pattern]] and the substring "{1}" replaced with dateFormat.[[pattern]].
        let time_format_pattern = time_format_obj.get("pattern", context)?;
        let time_format_pattern = time_format_pattern.to_string(context)?;
        let time_format_pattern = time_format_pattern.to_string();

        let date_format_pattern = date_format_obj.get("pattern", context)?;
        let date_format_pattern = date_format_pattern.to_string(context)?;
        let date_format_pattern = date_format_pattern.to_string();

        let pattern = connector_str.replace("{0}", &time_format_pattern);
        let pattern = pattern.replace("{1}", &date_format_pattern);

        // f. Set format.[[pattern]] to pattern.
        format.set(
            "pattern",
            JsValue::String(JsString::new(pattern)),
            true,
            context,
        )?;

        // g. If timeFormat has a [[pattern12]] field, then
        if let Ok(pattern12) = time_format_obj.get("pattern12", context) {
            // i. Let pattern12 be the string connector with the substring "{0}"
            // replaced with timeFormat.[[pattern12]] and the substring "{1}" replaced with
            // dateFormat.[[pattern]].
            let pattern12_str = pattern12
                .to_string(context)
                .unwrap_or_else(|_| JsString::empty());
            let pattern12_str = pattern12_str.to_string();

            let date_format_pattern = date_format_obj.get("pattern", context)?;
            let date_format_pattern = date_format_pattern.to_string(context)?;
            let date_format_pattern = date_format_pattern.to_string();

            let pattern12 = connector_str.replace("{0}", &pattern12_str);
            let pattern12 = pattern12.replace("{1}", &date_format_pattern);

            // ii. Set format.[[pattern12]] to pattern12.
            format.set(
                "pattern12",
                JsValue::String(JsString::new(pattern12)),
                true,
                context,
            )?;
        }

        // h. Let dateTimeRangeFormat be styles.[[DateTimeRangeFormat]].[[<dateStyle>]].[[<timeStyle>]].
        let date_style_str = date_style
            .to_string(context)
            .unwrap_or_else(|_| JsString::empty());
        let date_style_str = date_style_str.to_string();
        let time_style_str = time_style
            .to_string(context)
            .unwrap_or_else(|_| JsString::empty());
        let time_style_str = time_style_str.to_string();

        let date_time_range_format = styles.get("DateTimeRangeFormat", context)?;
        let date_time_range_format = date_time_range_format.to_object(context)?;
        let date_time_range_format = date_time_range_format.get(date_style_str, context)?;
        let date_time_range_format = date_time_range_format.to_object(context)?;
        let date_time_range_format = date_time_range_format.get(time_style_str, context)?;
        let date_time_range_format = date_time_range_format.to_object(context)?;

        // i. Set format.[[rangePatterns]] to dateTimeRangeFormat.[[rangePatterns]].
        format.set(
            "rangePatterns",
            date_time_range_format.get("rangePatterns", context)?,
            true,
            context,
        )?;

        // j. If dateTimeRangeFormat has a [[rangePatterns12]] field, then
        if let Ok(range_patterns12) = date_time_range_format.get("rangePatterns12", context) {
            // i. Set format.[[rangePatterns12]] to dateTimeRangeFormat.[[rangePatterns12]].
            format.set("rangePatterns12", range_patterns12, true, context)?;
        }

        // k. Return format.
        return Ok(JsValue::Object(format));
    }

    // 4. If timeStyle is not undefined, then
    if !time_style.is_undefined() {
        // a. Return timeFormat.
        return Ok(time_format);
    }

    // 5. Assert: dateStyle is not undefined.
    if date_style.is_undefined() {
        return context
            .throw_type_error("DateTimeStyleFormat: date style must be defined at this point.");
    }

    Ok(date_format)
}

/// `BasicFormatMatcher` abstract operation is called with two arguments `options` and `formats`
///
/// More information:
///  - [ECMAScript reference][spec]
///
/// [spec]: https://tc39.es/ecma402/#sec-basicformatmatcher
fn basic_format_matcher(
    options: &FormatOptionsRecord,
    formats: &JsValue,
    context: &mut Context,
) -> JsResult<JsValue> {
    // FIXME untested
    // 1. Let removalPenalty be 120.
    let removal_penalty = 120;

    // 2. Let additionPenalty be 20.
    let addition_penalty = 20;

    // 3. Let longLessPenalty be 8.
    let long_less_penalty = 8;

    // 4. Let longMorePenalty be 6.
    let long_more_penalty = 6;

    // 5. Let shortLessPenalty be 6.
    let short_less_penalty = 6;

    // 6. Let shortMorePenalty be 3.
    let short_more_penalty = 3;

    // 7. Let offsetPenalty be 1.
    let offset_penalty = 1;

    // 8. Let bestScore be -Infinity.
    let mut best_score = i32::MIN;

    // 9. Let bestFormat be undefined.
    let mut best_format = JsValue::undefined();

    // 10. Assert: Type(formats) is List.
    if !formats.is_array(context).unwrap_or(false) {
        return context.throw_type_error("Formats must be an array");
    }

    let formats = formats
        .to_object(context)
        .expect("Formats must be an object");

    // 11. For each element format of formats, do
    let formats_len = formats.length_of_array_like(context).unwrap_or(0);

    for idx in 0..formats_len {
        let format = formats
            .get(idx, context)
            .unwrap_or_else(|_| JsValue::undefined());
        let format = format
            .to_object(context)
            .unwrap_or_else(|_| JsObject::empty());

        // a. Let score be 0.
        let mut score = 0;

        // b. For each property name property shown in Table 6, do
        let date_time_components = build_date_time_components();
        for table_row in date_time_components {
            let property = table_row.property.to_string();
            // i. If options has a field [[<property>]],
            // let optionsProp be options.[[<property>]];
            // else let optionsProp be undefined.
            let options_prop = match options.properties.get(&table_row.property) {
                Some(opt) => opt.clone(),
                None => JsValue::undefined(),
            };

            // ii. If format has a field [[<property>]],
            // let formatProp be format.[[<property>]];
            // else let formatProp be undefined.
            let format_prop = format
                .get(property.clone(), context)
                .unwrap_or_else(|_| JsValue::undefined());

            // iii. If optionsProp is undefined and formatProp is not undefined,
            // decrease score by additionPenalty.
            if options_prop.is_undefined() && !format_prop.is_undefined() {
                score -= addition_penalty;

            // iv. Else if optionsProp is not undefined and formatProp is undefined,
            // decrease score by removalPenalty.
            } else if !options_prop.is_undefined() && format_prop.is_undefined() {
                score -= removal_penalty;
            // v. Else if property is "timeZoneName", then
            } else if property.eq("timeZoneName") {
                // 1. If optionsProp is "short" or "shortGeneric", then
                if options_prop.eq(&JsValue::String(JsString::new("short")))
                    || options_prop.eq(&JsValue::String(JsString::new("shortGeneric")))
                {
                    // a. If formatProp is "shortOffset", decrease score by offsetPenalty.
                    // b. Else if formatProp is "longOffset",
                    // decrease score by (offsetPenalty + shortMorePenalty).
                    // c. Else if optionsProp is "short" and formatProp is "long",
                    // decrease score by shortMorePenalty.
                    // d. Else if optionsProp is "shortGeneric" and formatProp is "longGeneric",
                    // decrease score by shortMorePenalty.
                    // e. Else if optionsProp ≠ formatProp, decrease score by removalPenalty.
                    if format_prop.eq(&JsValue::String(JsString::new("shortOffset"))) {
                        // a.
                        score -= offset_penalty;
                    } else if format_prop.eq(&JsValue::String(JsString::new("longOffset"))) {
                        // b.
                        score -= offset_penalty + short_more_penalty;
                    } else if (options_prop.eq(&JsValue::String(JsString::new("short")))
                        && format_prop.eq(&JsValue::String(JsString::new("long"))))
                        || (options_prop.eq(&JsValue::String(JsString::new("shortGeneric")))
                            && format_prop.eq(&JsValue::String(JsString::new("longGeneric"))))
                    {
                        // c & d.
                        score -= short_more_penalty;
                    } else if options_prop.ne(&format_prop) {
                        // e.
                        score -= removal_penalty;
                    }

                // 2. Else if optionsProp is "shortOffset" and formatProp is "longOffset",
                // decrease score by shortMorePenalty.
                } else if options_prop.eq(&JsValue::String(JsString::new("shortOffset")))
                    || format_prop.eq(&JsValue::String(JsString::new("longOffset")))
                {
                    score -= short_more_penalty;

                // 3. Else if optionsProp is "long" or "longGeneric", then
                } else if options_prop.eq(&JsValue::String(JsString::new("long")))
                    || options_prop.eq(&JsValue::String(JsString::new("longGeneric")))
                {
                    // a. If formatProp is "longOffset", decrease score by offsetPenalty.
                    // b. Else if formatProp is "shortOffset",
                    // decrease score by (offsetPenalty + longLessPenalty).
                    // c. Else if optionsProp is "long" and formatProp is "short",
                    // decrease score by longLessPenalty.
                    // d. Else if optionsProp is "longGeneric" and formatProp is "shortGeneric",
                    // decrease score by longLessPenalty.
                    // e. Else if optionsProp ≠ formatProp, decrease score by removalPenalty.
                    if format_prop.eq(&JsValue::String(JsString::new("longOffset"))) {
                        // a.
                        score -= offset_penalty;
                    } else if format_prop.eq(&JsValue::String(JsString::new("shortOffset"))) {
                        // b.
                        score -= offset_penalty + long_less_penalty;
                    } else if (options_prop.eq(&JsValue::String(JsString::new("long")))
                        && format_prop.eq(&JsValue::String(JsString::new("short"))))
                        || (options_prop.eq(&JsValue::String(JsString::new("longGeneric")))
                            && format_prop.eq(&JsValue::String(JsString::new("shortGeneric"))))
                    {
                        // c & d.
                        score -= long_less_penalty;
                    } else if options_prop.ne(&format_prop) {
                        // e.
                        score -= removal_penalty;
                    }

                // 4. Else if optionsProp is "longOffset" and formatProp is "shortOffset",
                // decrease score by longLessPenalty.
                } else if options_prop.eq(&JsValue::String(JsString::new("longOffset")))
                    || format_prop.eq(&JsValue::String(JsString::new("shortOffset")))
                {
                    score -= long_less_penalty;

                // 5. Else if optionsProp ≠ formatProp, decrease score by removalPenalty.
                } else if options_prop.ne(&format_prop) {
                    score -= removal_penalty;
                }

            // vi. Else if optionsProp ≠ formatProp, then
            } else if options_prop.ne(&format_prop) {
                // 1. If property is "fractionalSecondDigits", then
                //     a. Let values be « 1𝔽, 2𝔽, 3𝔽 ».
                // 2. Else,
                //     a. Let values be « "2-digit", "numeric", "narrow", "short", "long" ».
                let values = if property.eq("fractionalSecondDigits") {
                    vec![JsValue::new(1.0), JsValue::new(2.0), JsValue::new(3.0)]
                } else {
                    vec![
                        JsValue::String(JsString::new("2-digit")),
                        JsValue::String(JsString::new("numeric")),
                        JsValue::String(JsString::new("narrow")),
                        JsValue::String(JsString::new("short")),
                        JsValue::String(JsString::new("long")),
                    ]
                };

                // 3. Let optionsPropIndex be the index of optionsProp within values.
                let options_prop_index = values
                    .iter()
                    .position(|val| val.eq(&options_prop))
                    .expect("Option not found") as i32;

                // 4. Let formatPropIndex be the index of formatProp within values.
                let format_prop_index = values
                    .iter()
                    .position(|val| val.eq(&format_prop))
                    .expect("Format not found") as i32;

                // 5. Let delta be max(min(formatPropIndex - optionsPropIndex, 2), -2).
                let delta = max(min(format_prop_index - options_prop_index, 2), -2);

                // 6. If delta = 2, decrease score by longMorePenalty.
                // 7. Else if delta = 1, decrease score by shortMorePenalty.
                // 8. Else if delta = -1, decrease score by shortLessPenalty.
                // 9. Else if delta = -2, decrease score by longLessPenalty.
                if delta == 2 {
                    score -= long_more_penalty;
                } else if delta == 1 {
                    score -= short_more_penalty;
                } else if delta == -1 {
                    score -= short_less_penalty;
                } else if delta == -2 {
                    score -= long_less_penalty;
                }
            }
        }

        // c. If score > bestScore, then
        if score > best_score {
            // i. Let bestScore be score.
            best_score = score;

            // ii. Let bestFormat be format.
            best_format = JsValue::Object(format);
        }
    }

    // 12. Return bestFormat.
    Ok(best_format)
}

/// When the `BestFitFormatMatcher` abstract operation is called with two arguments `options` and
/// `formats`, it performs implementation dependent steps, which should return a set of component
/// representations that a typical user of the selected locale would perceive as at least as good
/// as the one returned by `BasicFormatMatcher`.
///
/// More information:
///  - [ECMAScript reference][spec]
///
/// [spec]: https://tc39.es/ecma402/#sec-bestfitformatmatcher
fn best_fit_format_matcher(
    options: &FormatOptionsRecord,
    formats: &JsValue,
    context: &mut Context,
) -> JsResult<JsValue> {
    basic_format_matcher(options, formats, context)
}

fn build_date_time_components() -> Vec<DateTimeComponents> {
    vec![
        DateTimeComponents {
            internal_slot: JsString::new("Weekday"),
            property: JsString::new("weekday"),
            values: vec![
                JsValue::String(JsString::new("narrow")),
                JsValue::String(JsString::new("short")),
                JsValue::String(JsString::new("long")),
            ],
        },
        DateTimeComponents {
            internal_slot: JsString::new("Era"),
            property: JsString::new("era"),
            values: vec![
                JsValue::String(JsString::new("narrow")),
                JsValue::String(JsString::new("short")),
                JsValue::String(JsString::new("long")),
            ],
        },
        DateTimeComponents {
            internal_slot: JsString::new("Year"),
            property: JsString::new("year"),
            values: vec![
                JsValue::String(JsString::new("2-digit")),
                JsValue::String(JsString::new("numeric")),
            ],
        },
        DateTimeComponents {
            internal_slot: JsString::new("Month"),
            property: JsString::new("month"),
            values: vec![
                JsValue::String(JsString::new("2-digit")),
                JsValue::String(JsString::new("numeric")),
                JsValue::String(JsString::new("narrow")),
                JsValue::String(JsString::new("short")),
                JsValue::String(JsString::new("long")),
            ],
        },
        DateTimeComponents {
            internal_slot: JsString::new("Day"),
            property: JsString::new("day"),
            values: vec![
                JsValue::String(JsString::new("2-digit")),
                JsValue::String(JsString::new("numeric")),
            ],
        },
        DateTimeComponents {
            internal_slot: JsString::new("DayPeriod"),
            property: JsString::new("dayPeriod"),
            values: vec![
                JsValue::String(JsString::new("narrow")),
                JsValue::String(JsString::new("short")),
                JsValue::String(JsString::new("long")),
            ],
        },
        DateTimeComponents {
            internal_slot: JsString::new("Hour"),
            property: JsString::new("hour"),
            values: vec![
                JsValue::String(JsString::new("narrow")),
                JsValue::String(JsString::new("short")),
                JsValue::String(JsString::new("long")),
            ],
        },
        DateTimeComponents {
            internal_slot: JsString::new("Minute"),
            property: JsString::new("minute"),
            values: vec![
                JsValue::String(JsString::new("narrow")),
                JsValue::String(JsString::new("short")),
                JsValue::String(JsString::new("long")),
            ],
        },
        DateTimeComponents {
            internal_slot: JsString::new("Second"),
            property: JsString::new("second"),
            values: vec![
                JsValue::String(JsString::new("narrow")),
                JsValue::String(JsString::new("short")),
                JsValue::String(JsString::new("long")),
            ],
        },
        DateTimeComponents {
            internal_slot: JsString::new("FractionalSecondDigits"),
            property: JsString::new("fractionalSecondDigits"),
            values: vec![JsValue::new(1.0), JsValue::new(2.0), JsValue::new(3.0)],
        },
        DateTimeComponents {
            internal_slot: JsString::new("TimeZoneName"),
            property: JsString::new("timeZoneName"),
            values: vec![
                JsValue::String(JsString::new("short")),
                JsValue::String(JsString::new("long")),
                JsValue::String(JsString::new("shortOffset")),
                JsValue::String(JsString::new("longOffset")),
                JsValue::String(JsString::new("shortGeneric")),
                JsValue::String(JsString::new("longGeneric")),
            ],
        },
    ]
}

/// The abstract operation `InitializeDateTimeFormat` accepts the arguments `dateTimeFormat` (which
/// must be an object), `locales`, and `options`. It initializes `dateTimeFormat` as a
/// `DateTimeFormat` object.
///
/// More information:
///  - [ECMAScript reference][spec]
///
/// [spec]: https://tc39.es/ecma402/#sec-initializedatetimeformat
fn initialize_date_time_format(
    date_time_format: &JsObject,
    locales: &JsValue,
    options: &JsValue,
    context: &mut Context,
) -> JsResult<JsObject> {
    // 1. Let requestedLocales be ? CanonicalizeLocaleList(locales).
    let locales_arr = if locales.is_undefined() {
        vec![JsValue::undefined()]
    } else {
        let locales_obj = locales
            .to_object(context)
            .unwrap_or_else(|_| JsObject::empty());
        let locales_len = locales_obj.length_of_array_like(context).unwrap_or(0);
        let mut locales_acc = Vec::<JsValue>::new();
        for index in 0..locales_len as u32 {
            let maybe_locale = locales_obj
                .get(index, context)
                .unwrap_or_else(|_| JsValue::undefined());
            locales_acc.push(maybe_locale);
        }
        locales_acc
    };
    let requested_locales =
        Intl::canonicalize_locale_list(&locales_arr, context).unwrap_or_default();

    // 2. Set options to ? ToDateTimeOptions(options, "any", "date").
    let options =
        to_date_time_options(options, "any", "date", context).unwrap_or_else(|_| JsObject::empty());

    // 3. Let opt be a new Record.
    let mut opt = DateTimeFormatRecord {
        locale_matcher: JsString::empty(),
        properties: FxHashMap::default(),
    };

    let options_obj = JsValue::Object(options.clone());

    // 4. Let matcher be ? GetOption(options, "localeMatcher", "string", « "lookup", "best fit" », "best fit").
    let matcher_values = vec![
        JsValue::String(JsString::new("lookup")),
        JsValue::String(JsString::new("best fit")),
    ];
    let matcher = get_option(
        &options_obj,
        "localeMatcher",
        "string",
        &matcher_values,
        &JsValue::String(JsString::new("best fit")),
        context,
    )
    .unwrap_or_else(|_| JsValue::undefined());

    // 5. Set opt.[[localeMatcher]] to matcher.
    opt.locale_matcher = matcher
        .to_string(context)
        .unwrap_or_else(|_| JsString::empty());

    // 6. Let calendar be ? GetOption(options, "calendar", "string", undefined, undefined).
    let calendar = get_option(
        &options_obj,
        "calendar",
        "string",
        &Vec::<JsValue>::new(),
        &JsValue::undefined(),
        context,
    )
    .unwrap_or_else(|_| JsValue::undefined());

    // 7. If calendar is not undefined, then
    if !calendar.is_undefined() {
        // a. If calendar does not match the Unicode Locale Identifier type nonterminal,
        // throw a RangeError exception.
        if !is_nonterminal(&calendar, context) {
            return context.throw_range_error("calendar must be nonterminal");
        }
    }

    // 8. Set opt.[[ca]] to calendar.
    opt.properties.insert(JsString::new("ca"), calendar);

    // 9. Let numberingSystem be ? GetOption(options, "numberingSystem", "string", undefined, undefined).
    let numbering_system = get_option(
        &options_obj,
        "numberingSystem",
        "string",
        &Vec::<JsValue>::new(),
        &JsValue::undefined(),
        context,
    )
    .unwrap_or_else(|_| JsValue::undefined());

    // 10. If numberingSystem is not undefined, then
    if !numbering_system.is_undefined() {
        // a. If numberingSystem does not match the Unicode Locale Identifier type nonterminal,
        // throw a RangeError exception.
        if !is_nonterminal(&numbering_system, context) {
            return context.throw_range_error("numberingSystem must be nonterminal");
        }
    }

    // 11. Set opt.[[nu]] to numberingSystem.
    opt.properties.insert(JsString::new("nu"), numbering_system);

    // 12. Let hour12 be ? GetOption(options, "hour12", "boolean", undefined, undefined).
    let hour_12 = get_option(
        &options_obj,
        "hour12",
        "boolean",
        &Vec::<JsValue>::new(),
        &JsValue::undefined(),
        context,
    )
    .unwrap_or_else(|_| JsValue::undefined());

    // 13. Let hourCycle be ? GetOption(options, "hourCycle", "string", « "h11", "h12", "h23", "h24" », undefined).
    let hour_cycle_values = vec![
        JsValue::String(JsString::new("h11")),
        JsValue::String(JsString::new("h12")),
        JsValue::String(JsString::new("h23")),
        JsValue::String(JsString::new("h24")),
    ];
    let mut hour_cycle = get_option(
        &options_obj,
        "hourCycle",
        "string",
        &hour_cycle_values,
        &JsValue::undefined(),
        context,
    )
    .unwrap_or_else(|_| JsValue::undefined());

    // 14. If hour12 is not undefined, then
    if !hour_12.is_undefined() {
        // a. Set hourCycle to null.
        hour_cycle = JsValue::null();
    }

    // 15. Set opt.[[hc]] to hourCycle.
    opt.properties.insert(JsString::new("hc"), hour_cycle);

    // 16. Let localeData be %DateTimeFormat%.[[LocaleData]].
    let locale_data = build_locale_data(context);
    let relevant_ext_keys = build_relevant_ext_keys();
    let available_locales = build_available_locales();

    // 17. Let r be ResolveLocale(%DateTimeFormat%.[[AvailableLocales]], requestedLocales, opt, %DateTimeFormat%.[[RelevantExtensionKeys]], localeData).
    let r = resolve_locale(
        &available_locales,
        &requested_locales,
        &opt,
        &relevant_ext_keys,
        &locale_data,
        context,
    );

    let mut date_time_fmt_borrow = date_time_format.borrow_mut();
    let date_time_fmt = date_time_fmt_borrow
        .as_date_time_format_mut()
        .expect("Cast to DateTimeFormat failed");
    // 18. Set dateTimeFormat.[[Locale]] to r.[[locale]].
    date_time_fmt.locale = r.locale.clone();

    // 19. Let resolvedCalendar be r.[[ca]].
    let resolved_calendar = r
        .properties
        .get(&JsString::new("ca"))
        .expect("Failed to resolve calendar");
    // 20. Set dateTimeFormat.[[Calendar]] to resolvedCalendar.
    date_time_fmt.calendar = resolved_calendar.clone();

    let resolved_calendar_str = resolved_calendar
        .to_string(context)
        .expect("Failed to cast resolved calendar to string");
    let resolved_calendar_str = resolved_calendar_str.to_string();

    // 21. Set dateTimeFormat.[[NumberingSystem]] to r.[[nu]].
    let resolved_nu = r
        .properties
        .get(&JsString::new("nu"))
        .expect("Failed to resolve numbering system");
    date_time_fmt.numbering_system = resolved_nu.clone();

    // 22. Let dataLocale be r.[[dataLocale]].
    let data_locale = r.data_locale;

    // 23. Let dataLocaleData be localeData.[[<dataLocale>]].
    let data_locale_data = locale_data
        .get(&data_locale)
        .expect("Failed to resolve data locale");

    // 24. Let hcDefault be dataLocaleData.[[hourCycle]].
    let hc_default = data_locale_data
        .get(&JsString::new("hourCycle"))
        .expect("Failed to resolve hour cycle");

    // 25. If hour12 is true, then
    // a. If hcDefault is "h11" or "h23", let hc be "h11". Otherwise, let hc be "h12".
    // 26. Else if hour12 is false, then
    // a. If hcDefault is "h11" or "h23", let hc be "h23". Otherwise, let hc be "h24".
    // 27. Else,
    // a. Assert: hour12 is undefined.
    // b. Let hc be r.[[hc]].
    // c. If hc is null, set hc to hcDefault.
    let hc = if hour_12.is_boolean() {
        if hour_12.to_boolean() {
            if hc_default.eq(&JsValue::String(JsString::new("h11")))
                || hc_default.eq(&JsValue::String(JsString::new("h23")))
            {
                JsValue::String(JsString::new("h11"))
            } else {
                JsValue::String(JsString::new("h12"))
            }
        } else if hc_default.eq(&JsValue::String(JsString::new("h11")))
            || hc_default.eq(&JsValue::String(JsString::new("h23")))
        {
            JsValue::String(JsString::new("h23"))
        } else {
            JsValue::String(JsString::new("h24"))
        }
    } else {
        let hc_prop = r
            .properties
            .get(&JsString::new("hc"))
            .expect("Failed to resolve hc");
        hc_prop.clone()
    };

    // 28. Set dateTimeFormat.[[HourCycle]] to hc.
    date_time_fmt.hour_cycle = hc.clone();

    // 29. Let timeZone be ? Get(options, "timeZone").
    let time_zone = options
        .get("timeZone", context)
        .unwrap_or_else(|_| JsValue::undefined());

    // 30. If timeZone is undefined, then
    //     a. Set timeZone to ! DefaultTimeZone().
    // 31. Else,
    //     a. Set timeZone to ? ToString(timeZone).
    //     b. If the result of ! IsValidTimeZoneName(timeZone) is false, then
    //         i. Throw a RangeError exception.
    //     c. Set timeZone to ! CanonicalizeTimeZoneName(timeZone).
    let time_zone_str = if time_zone.is_undefined() {
        // FIXME implement DefaultTimeZone()
        JsValue::String(JsString::new("UTC"))
    } else {
        // FIXME implement CanonicalizeTimeZoneName() and IsValidTimeZoneName()
        JsValue::String(
            time_zone
                .to_string(context)
                .unwrap_or_else(|_| JsString::new("UTC")),
        )
    };

    // 32. Set dateTimeFormat.[[TimeZone]] to timeZone.
    date_time_fmt.time_zone = time_zone_str;

    // 33. Let formatOptions be a new Record.
    let mut format_options = FormatOptionsRecord {
        hour_cycle: JsValue::undefined(),
        properties: FxHashMap::default(),
    };

    // 34. Set formatOptions.[[hourCycle]] to hc.
    format_options.hour_cycle = hc;

    // 35. Let hasExplicitFormatComponents be false.
    let mut has_explicit_format_components = false;

    // 36. For each row of Table 6, except the header row, in table order, do
    let date_time_components = build_date_time_components();

    for table_row in date_time_components {
        // a. Let prop be the name given in the Property column of the row.
        let prop = table_row.property;

        // b. If prop is "fractionalSecondDigits", then
        //      i. Let value be ? GetNumberOption(options, "fractionalSecondDigits", 1, 3,
        //      undefined).
        // c. Else,
        //      i. Let values be a List whose elements are the strings given in the Values
        //      column of the row.
        //      ii. Let value be ? GetOption(options, prop, "string", values, undefined).
        let value = if prop.eq("fractionalSecondDigits") {
            get_number_option(
                &options_obj,
                "fractionalSecondDigits",
                &JsValue::new(1),
                &JsValue::new(3),
                &JsValue::undefined(),
                context,
            )
            .unwrap_or_else(|_| JsValue::undefined())
        } else {
            let values = table_row.values;
            get_option(
                &options_obj,
                &prop,
                "string",
                &values,
                &JsValue::undefined(),
                context,
            )
            .unwrap_or_else(|_| JsValue::undefined())
        };

        // d. Set formatOptions.[[<prop>]] to value.
        // e. If value is not undefined, then
        if !value.is_undefined() {
            // i. Set hasExplicitFormatComponents to true.
            has_explicit_format_components = true;
        }
        format_options.properties.insert(prop, value);
    }

    // 37. Let matcher be ? GetOption(options, "formatMatcher", "string", « "basic", "best fit" »,
    // "best fit").
    let matcher_values = vec![
        JsValue::String(JsString::new("basic")),
        JsValue::String(JsString::new("best fit")),
    ];
    let matcher = get_option(
        &options_obj,
        "formatMatcher",
        "string",
        &matcher_values,
        &JsValue::String(JsString::new("best fit")),
        context,
    )
    .unwrap_or_else(|_| JsValue::undefined());

    // 38. Let dateStyle be ? GetOption(options, "dateStyle", "string",
    // « "full", "long", "medium", "short" », undefined).
    let date_style_values = vec![
        JsValue::String(JsString::new("full")),
        JsValue::String(JsString::new("long")),
        JsValue::String(JsString::new("medium")),
        JsValue::String(JsString::new("short")),
    ];
    let date_style = get_option(
        &options_obj,
        "dateStyle",
        "string",
        &date_style_values,
        &JsValue::undefined(),
        context,
    )
    .unwrap_or_else(|_| JsValue::undefined());

    // 39. Set dateTimeFormat.[[DateStyle]] to dateStyle.
    date_time_fmt.date_style = date_style.clone();

    // 40. Let timeStyle be ? GetOption(options, "timeStyle", "string",
    // « "full", "long", "medium", "short" », undefined).
    let time_style_values = vec![
        JsValue::String(JsString::new("full")),
        JsValue::String(JsString::new("long")),
        JsValue::String(JsString::new("medium")),
        JsValue::String(JsString::new("short")),
    ];
    let time_style = get_option(
        &options_obj,
        "timeStyle",
        "string",
        &time_style_values,
        &JsValue::undefined(),
        context,
    )
    .unwrap_or_else(|_| JsValue::undefined());

    // 41. Set dateTimeFormat.[[TimeStyle]] to timeStyle.
    date_time_fmt.time_style = time_style.clone();

    // 42. If dateStyle is not undefined or timeStyle is not undefined, then
    let best_format = if !date_style.is_undefined() || !time_style.is_undefined() {
        // a. If hasExplicitFormatComponents is true, then
        if has_explicit_format_components {
            // i. Throw a TypeError exception.
            return context.throw_type_error(
                "dateStyle or timeStyle is defined, while components have explicit format",
            );
        }

        // b. Let styles be dataLocaleData.[[styles]].[[<resolvedCalendar>]].
        let date_locale_styles = data_locale_data
            .get(&JsString::new("styles"))
            .expect("Failed to resolve styles");
        let date_locale_styles = date_locale_styles
            .to_object(context)
            .expect("Failed to cast date locale styles to object");
        let styles = date_locale_styles
            .get(resolved_calendar_str, context)
            .expect("Failed to resolve calendar styles");
        let styles = styles
            .to_object(context)
            .expect("Failed to cast styles to object");

        // c. Let bestFormat be DateTimeStyleFormat(dateStyle, timeStyle, styles).
        date_time_style_format(&date_style, &time_style, &styles, context)
            .expect("Failed to build DateTimeStyleFormat")
    // 43. Else,
    } else {
        // a. Let formats be dataLocaleData.[[formats]].[[<resolvedCalendar>]].
        let date_locale_formats = data_locale_data
            .get(&JsString::new("formats"))
            .expect("Failed to resolve formats");
        let date_locale_formats = date_locale_formats
            .to_object(context)
            .expect("Failed to cast formats to object");
        let formats = date_locale_formats
            .get(resolved_calendar_str, context)
            .expect("Failed to resolve calendar");

        // b. If matcher is "basic", then
        if matcher.eq(&JsValue::String(JsString::new("basic"))) {
            // i. Let bestFormat be BasicFormatMatcher(formatOptions, formats).
            basic_format_matcher(&format_options, &formats, context)
                .expect("Failed to get basic format")
        // c. Else,
        } else {
            // i. Let bestFormat be BestFitFormatMatcher(formatOptions, formats).
            best_fit_format_matcher(&format_options, &formats, context)
                .expect("Failed to get best fit format")
        }
    };

    // 44. For each row in Table 6, except the header row, in table order, do
    let date_time_components = build_date_time_components();

    for table_row in date_time_components {
        // a. Let prop be the name given in the Property column of the row.
        let prop = table_row.property;

        // b. If bestFormat has a field [[<prop>]], then
        let best_format_obj = best_format
            .to_object(context)
            .expect("Failed to cast bestFormat to object");
        if let Ok(best_format_prop) = best_format_obj.get(prop, context) {
            // i. Let p be bestFormat.[[<prop>]].
            // ii. Set dateTimeFormat's internal slot whose name is the Internal Slot column
            // of the row to p.
            if table_row.internal_slot.eq(&JsString::new("Weekday")) {
                date_time_fmt.weekday = best_format_prop;
            } else if table_row.internal_slot.eq(&JsString::new("Era")) {
                date_time_fmt.era = best_format_prop;
            } else if table_row.internal_slot.eq(&JsString::new("Year")) {
                date_time_fmt.year = best_format_prop;
            } else if table_row.internal_slot.eq(&JsString::new("Month")) {
                date_time_fmt.month = best_format_prop;
            } else if table_row.internal_slot.eq(&JsString::new("Day")) {
                date_time_fmt.day = best_format_prop;
            } else if table_row.internal_slot.eq(&JsString::new("DayPeriod")) {
                date_time_fmt.day_period = best_format_prop;
            } else if table_row.internal_slot.eq(&JsString::new("Hour")) {
                date_time_fmt.hour = best_format_prop;
            } else if table_row.internal_slot.eq(&JsString::new("Minute")) {
                date_time_fmt.minute = best_format_prop;
            } else if table_row.internal_slot.eq(&JsString::new("Second")) {
                date_time_fmt.second = best_format_prop;
            } else if table_row
                .internal_slot
                .eq(&JsString::new("FractionalSecondDigits"))
            {
                date_time_fmt.fractional_second_digits = best_format_prop;
            } else if table_row.internal_slot.eq(&JsString::new("TimeZoneName")) {
                date_time_fmt.time_zone_name = best_format_prop;
            }
        }
    }

    // 45. If dateTimeFormat.[[Hour]] is undefined, then
    if date_time_fmt.hour.is_undefined() {
        // a. Set dateTimeFormat.[[HourCycle]] to undefined.
        date_time_fmt.hour_cycle = JsValue::undefined();
    }

    // 46. If dateTimeformat.[[HourCycle]] is "h11" or "h12", then
    //      a. Let pattern be bestFormat.[[pattern12]].
    //      b. Let rangePatterns be bestFormat.[[rangePatterns12]].
    // 47. Else,
    //      a. Let pattern be bestFormat.[[pattern]].
    //      b. Let rangePatterns be bestFormat.[[rangePatterns]].
    let patterns_to_fetch = if date_time_fmt
        .hour_cycle
        .eq(&JsValue::String(JsString::new("h11")))
        || date_time_fmt
            .hour_cycle
            .eq(&JsValue::String(JsString::new("h12")))
    {
        vec!["pattern12", "rangePatterns12"]
    } else {
        vec!["pattern", "rangePatterns"]
    };

    let best_format_obj = best_format
        .to_object(context)
        .expect("Failed to cast bestFormat to object");
    let pattern = best_format_obj
        .get(patterns_to_fetch[0], context)
        .expect("Failed to get pattern field");
    let range_patterns = best_format_obj
        .get(patterns_to_fetch[1], context)
        .expect("Failed to get rangePatterns field");

    // 48. Set dateTimeFormat.[[Pattern]] to pattern.
    date_time_fmt.pattern = pattern;

    // 49. Set dateTimeFormat.[[RangePatterns]] to rangePatterns.
    date_time_fmt.range_patterns = range_patterns;

    // 50. Return dateTimeFormat.
    Ok(date_time_format.clone())
}
