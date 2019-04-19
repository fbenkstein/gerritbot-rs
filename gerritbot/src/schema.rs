table! {
    users (id) {
        id -> Integer,
        email -> Text,
        person_id -> Text,
        enabled -> Integer,
        filter -> Nullable<Text>,
    }
}
