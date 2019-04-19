use diesel::Queryable;

use gerritbot_spark as spark;

use super::schema::users;

#[derive(Queryable)]
pub struct User {
    pub id: u32,
    #[diesel(deserialize_as = "String")]
    pub email: spark::Email,
    #[diesel(deserialize_as = "String")]
    pub spark_person_id: spark::PersonId,
    pub enabled: bool,
    pub filter: Option<String>,
}

#[derive(Insertable)]
#[table_name = "users"]
pub struct NewUser<'a> {
    pub email: &'a str,
    pub person_id: &'a str,
}
