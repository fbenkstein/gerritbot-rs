use std::collections::HashMap;
use std::fs::File;
use std::path::Path;

use bitflags::bitflags;
use log::warn;
use num_derive::FromPrimitive;
use num_traits::FromPrimitive;
use regex::Regex;
use serde::{Deserialize, Serialize};

use gerritbot_spark as spark;

use super::BotError;

#[derive(Debug, PartialEq)]
pub enum AddFilterResult {
    UserNotFound,
    UserDisabled,
    InvalidFilter,
    FilterNotConfigured,
}

#[derive(Default, Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct Filter {
    pub regex: String,
    pub enabled: bool,
}

impl Filter {
    pub fn new<A: Into<String>>(regex: A) -> Self {
        Self {
            regex: regex.into(),
            enabled: true,
        }
    }
}

#[derive(Copy, Clone, Serialize, Deserialize, PartialEq, Eq, FromPrimitive)]
#[serde(rename_all = "snake_case")]
pub enum UserFlag {
    /// User wants notification messages for reviews with approvals.
    NotifyReviewApprovals = 0,
    /// User wants notification messages for review comments without approvals.
    NotifyReviewComments,
    /// User wants notification messages for reviews with inline comments.
    NotifyReviewInlineComments,
    /// User wants notification messages when added as reviewer to a change.
    NotifyReviewerAdded,
}

impl UserFlag {
    const fn to_flags(self) -> UserFlags {
        UserFlags {
            bits: 1 << self as usize,
        }
    }
}

impl Into<UserFlags> for UserFlag {
    fn into(self) -> UserFlags {
        self.to_flags()
    }
}

bitflags! {
    struct UserFlags: u16 {
        /// Notification marker for users that haven't specified which
        /// notifications they want. This is useful for backwards compatibility
        /// in the configuration (i.e. translating `true` to this value) and for
        /// being able to change the default and have it apply also to existing
        /// users.
        const DEFAULT_NOTIFICATIONS_MARKER  = 1 << 15;

        /// Default flags used when the user hasn't set any custom flags.
        const DEFAULT_NOTIFICATIONS = UserFlag::NotifyReviewApprovals.to_flags().bits
            | UserFlag::NotifyReviewInlineComments.to_flags().bits
            | UserFlag::NotifyReviewerAdded.to_flags().bits;

        /// All notification flags for reviews.
        const NOTIFY_ALL_REVIEW = UserFlag::NotifyReviewApprovals.to_flags().bits
            | UserFlag::NotifyReviewComments.to_flags().bits
            | UserFlag::NotifyReviewInlineComments.to_flags().bits;

        /// All notificaton flags.
        const NOTIFY_ALL_NOTIFICATIONS = Self::NOTIFY_ALL_REVIEW.bits
            | UserFlag::NotifyReviewerAdded.to_flags().bits;
    }
}

impl Iterator for UserFlags {
    type Item = UserFlag;

    fn next(&mut self) -> Option<Self::Item> {
        if self.contains(Self::DEFAULT_NOTIFICATIONS_MARKER) {
            self.remove(Self::DEFAULT_NOTIFICATIONS_MARKER);
            self.insert(Self::DEFAULT_NOTIFICATIONS);
        }

        while self.bits != 0 {
            let lowest_bit = self.bits.trailing_zeros() + 1;
            let lowest_bit_flags = UserFlags {
                bits: 1 << lowest_bit,
            };
            self.remove(lowest_bit_flags);
            let flag = UserFlag::from_u32(lowest_bit);

            if flag.is_some() {
                return flag;
            }
        }

        None
    }
}

impl UserFlags {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        if *self == Self::DEFAULT_NOTIFICATIONS_MARKER {
            serializer.serialize_bool(true)
        } else if self.is_empty() {
            serializer.serialize_bool(false)
        } else {
            serializer.collect_seq(*self)
        }
    }

    fn deserialize<'de, D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let value = serde_json::Value::deserialize(deserializer)?;

        match value {
            serde_json::Value::Bool(true) => Ok(Self::DEFAULT_NOTIFICATIONS_MARKER),
            serde_json::Value::Bool(false) => Ok(Self::empty()),
            _ => {
                use serde::de::Error as _;

                let flags =
                    serde_json::from_value::<Vec<UserFlag>>(value).map_err(D::Error::custom)?;

                Ok(flags
                    .into_iter()
                    .fold(Self::empty(), |flags, flag| flags | flag.into()))
            }
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct User {
    // Legacy attribute.  Keep so we don't drop it on deserialize, serialize.
    // Should be removed later.
    #[serde(skip_serializing_if = "Option::is_none")]
    spark_person_id: Option<String>,
    /// email of the user; assumed to be the same in Spark and Gerrit
    pub email: spark::Email,
    #[serde(
        rename = "enabled",
        serialize_with = "UserFlags::serialize",
        deserialize_with = "UserFlags::deserialize"
    )]
    flags: UserFlags,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub filter: Option<Filter>,
}

impl User {
    fn new(email: spark::Email) -> Self {
        Self {
            spark_person_id: None,
            email: email,
            filter: None,
            flags: UserFlags::DEFAULT_NOTIFICATIONS_MARKER,
        }
    }

    pub fn any_notifications_enabled(&self) -> bool {
        self.has_any_flag(UserFlags::NOTIFY_ALL_NOTIFICATIONS)
    }

    pub fn any_review_notifications_enabled(&self) -> bool {
        self.has_any_flag(UserFlags::NOTIFY_ALL_REVIEW)
    }

    /// Are any of the given user flags set?
    fn has_any_flag(&self, flags: UserFlags) -> bool {
        assert!(flags.bits.count_ones() > 0);
        self.flags.intersects(flags)
    }

    /// Is the given user flag set?
    pub fn has_flag(&self, flag: UserFlag) -> bool {
        self.has_any_flag(flag.into())
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, Default)]
pub struct State {
    users: Vec<User>,
    #[serde(skip_serializing, skip_deserializing)]
    email_index: HashMap<spark::Email, usize>,
}

impl State {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn load<P>(filename: P) -> Result<Self, BotError>
    where
        P: AsRef<Path>,
    {
        let f = File::open(filename)?;

        serde_json::from_reader(f)
            .map(|mut state: Self| {
                state.index_users();
                state
            })
            .map_err(BotError::from)
    }

    fn index_users(&mut self) {
        for (user_pos, user) in self.users.iter().enumerate() {
            self.email_index.insert(user.email.clone(), user_pos);
        }
    }

    pub fn num_users(&self) -> usize {
        self.users.len()
    }

    // Note: This method is not idempotent, and in particular, when adding the same user twice,
    // it will completely mess up the indexes.
    pub fn add_user<'a>(&'a mut self, email: &spark::EmailRef) -> &'a mut User {
        let user_pos = self.users.len();
        self.users.push(User::new(email.to_owned()));
        self.email_index.insert(email.to_owned(), user_pos);
        self.users.last_mut().unwrap()
    }

    fn find_or_add_user_by_email<'a>(&'a mut self, email: &spark::EmailRef) -> &'a mut User {
        let pos = self.users.iter().position(|u| u.email == email);
        let user: &'a mut User = match pos {
            Some(pos) => &mut self.users[pos],
            None => self.add_user(email),
        };
        user
    }

    fn find_user_mut<'a, P: ?Sized>(&'a mut self, email: &P) -> Option<&'a mut User>
    where
        spark::Email: std::borrow::Borrow<P>,
        P: std::hash::Hash + Eq,
    {
        self.email_index
            .get(email)
            .cloned()
            .map(move |pos| &mut self.users[pos])
    }

    pub fn find_user<'a, P: ?Sized>(&'a self, email: &P) -> Option<&'a User>
    where
        spark::Email: std::borrow::Borrow<P>,
        P: std::hash::Hash + Eq,
    {
        self.email_index
            .get(email)
            .cloned()
            .map(|pos| &self.users[pos])
    }

    pub fn find_user_by_email<'a, E: ?Sized>(&self, email: &E) -> Option<&User>
    where
        spark::Email: std::borrow::Borrow<E>,
        E: std::hash::Hash + Eq,
    {
        self.email_index.get(email).map(|pos| &self.users[*pos])
    }

    pub fn enable<'a>(&'a mut self, email: &spark::EmailRef, enabled: bool) -> &'a User {
        let user: &'a mut User = self.find_or_add_user_by_email(email);
        user.flags.insert(UserFlags::DEFAULT_NOTIFICATIONS_MARKER);
        user
    }

    pub fn add_filter<A>(
        &mut self,
        email: &spark::EmailRef,
        filter: A,
    ) -> Result<(), AddFilterResult>
    where
        A: Into<String>,
    {
        let user = self.find_user_mut(email);
        match user {
            Some(user) => {
                if !user.any_notifications_enabled() {
                    Err(AddFilterResult::UserDisabled)
                } else {
                    let filter: String = filter.into();
                    if Regex::new(&filter).is_err() {
                        return Err(AddFilterResult::InvalidFilter);
                    }
                    user.filter = Some(Filter::new(filter));
                    Ok(())
                }
            }
            None => Err(AddFilterResult::UserNotFound),
        }
    }

    pub fn get_filter<'a>(
        &'a self,
        email: &spark::EmailRef,
    ) -> Result<Option<&'a Filter>, AddFilterResult> {
        let user = self.find_user(email);
        match user {
            Some(user) => Ok(user.filter.as_ref()),
            None => Err(AddFilterResult::UserNotFound),
        }
    }

    pub fn enable_filter(
        &mut self,
        email: &spark::EmailRef,
        enabled: bool,
    ) -> Result<String /* filter */, AddFilterResult> {
        let user = self.find_user_mut(email);
        match user {
            Some(user) => {
                if !user.any_notifications_enabled() {
                    Err(AddFilterResult::UserDisabled)
                } else {
                    match user.filter.as_mut() {
                        Some(filter) => {
                            if Regex::new(&filter.regex).is_err() {
                                return Err(AddFilterResult::InvalidFilter);
                            }
                            filter.enabled = enabled;
                            Ok(filter.regex.clone())
                        }
                        None => Err(AddFilterResult::FilterNotConfigured),
                    }
                }
            }
            None => Err(AddFilterResult::UserNotFound),
        }
    }

    pub fn users(&self) -> impl Iterator<Item = &User> + Clone {
        self.users.iter()
    }

    pub fn is_filtered(&self, user: &User, msg: &str) -> bool {
        if let Some(filter) = user.filter.as_ref() {
            if filter.enabled {
                if let Ok(re) = Regex::new(&filter.regex) {
                    return re.is_match(msg);
                } else {
                    warn!(
                        "User {} has configured invalid filter regex: {}",
                        user.email, filter.regex
                    );
                }
            }
        }
        false
    }
}

#[cfg(test)]
mod test {
    use spark::EmailRef;

    use super::*;

    #[test]
    fn test_add_user() {
        let mut state = State::new();
        state.add_user(EmailRef::new("some@example.com"));
        assert_eq!(state.users.len(), 1);
        assert_eq!(state.email_index.len(), 1);
        assert_eq!(state.users[0].email, EmailRef::new("some@example.com"));
        assert_eq!(
            state.email_index.get(EmailRef::new("some@example.com")),
            Some(&0)
        );
        assert_eq!(
            state.email_index.get(EmailRef::new("some@example.com")),
            Some(&0)
        );

        state.add_user(EmailRef::new("some_2@example.com"));
        assert_eq!(state.users.len(), 2);
        assert_eq!(state.email_index.len(), 2);
        assert_eq!(state.users[1].email, EmailRef::new("some_2@example.com"));
        assert_eq!(
            state.email_index.get(EmailRef::new("some_2@example.com")),
            Some(&1)
        );
        assert_eq!(
            state.email_index.get(EmailRef::new("some_2@example.com")),
            Some(&1)
        );

        let user = state.find_user(EmailRef::new("some@example.com"));
        assert!(user.is_some());
        assert_eq!(user.unwrap().email, EmailRef::new("some@example.com"));

        let user = state.find_user(EmailRef::new("some_2@example.com"));
        assert!(user.is_some());
        assert_eq!(user.unwrap().email, EmailRef::new("some_2@example.com"));
    }

    #[test]
    fn add_invalid_filter_for_existing_user() {
        let mut state = State::new();
        state.add_user(EmailRef::new("some@example.com"));
        let res = state.add_filter(EmailRef::new("some@example.com"), ".some_weard_regex/[");
        assert_eq!(res, Err(AddFilterResult::InvalidFilter));
        assert!(state
            .users
            .iter()
            .position(|u| u.email == EmailRef::new("some@example.com") && u.filter == None)
            .is_some());

        let res = state.enable_filter(EmailRef::new("some@example.com"), true);
        assert_eq!(res, Err(AddFilterResult::FilterNotConfigured));
        let res = state.enable_filter(EmailRef::new("some@example.com"), false);
        assert_eq!(res, Err(AddFilterResult::FilterNotConfigured));
    }

    #[test]
    fn add_valid_filter_for_existing_user() {
        let mut state = State::new();
        state.add_user(EmailRef::new("some@example.com"));

        let res = state.add_filter(EmailRef::new("some@example.com"), ".*some_word.*");
        assert!(res.is_ok());
        assert!(state
            .users
            .iter()
            .position(|u| u.email == EmailRef::new("some@example.com")
                && u.filter == Some(Filter::new(".*some_word.*")))
            .is_some());

        {
            let filter = state.get_filter(EmailRef::new("some@example.com"));
            assert_eq!(filter, Ok(Some(&Filter::new(".*some_word.*"))));
        }
        let res = state.enable_filter(EmailRef::new("some@example.com"), false);
        assert_eq!(res, Ok(String::from(".*some_word.*")));
        assert!(state
            .users
            .iter()
            .position(|u| u.email == EmailRef::new("some@example.com")
                && u.filter.as_ref().map(|f| f.enabled) == Some(false))
            .is_some());
        {
            let filter = state
                .get_filter(EmailRef::new("some@example.com"))
                .unwrap()
                .unwrap();
            assert_eq!(filter.regex, ".*some_word.*");
            assert_eq!(filter.enabled, false);
        }
        let res = state.enable_filter(EmailRef::new("some@example.com"), true);
        assert_eq!(res, Ok(String::from(".*some_word.*")));
        assert!(state
            .users
            .iter()
            .position(|u| u.email == EmailRef::new("some@example.com")
                && u.filter.as_ref().map(|f| f.enabled) == Some(true))
            .is_some());
        {
            let filter = state.get_filter(EmailRef::new("some@example.com"));
            assert_eq!(filter, Ok(Some(&Filter::new(".*some_word.*"))));
        }
    }

    #[test]
    fn add_valid_filter_for_non_existing_user() {
        let mut state = State::new();
        let res = state.add_filter(EmailRef::new("some@example.com"), ".*some_word.*");
        assert_eq!(res, Err(AddFilterResult::UserNotFound));
        let res = state.enable_filter(EmailRef::new("some@example.com"), true);
        assert_eq!(res, Err(AddFilterResult::UserNotFound));
        let res = state.enable_filter(EmailRef::new("some@example.com"), false);
        assert_eq!(res, Err(AddFilterResult::UserNotFound));
    }

    #[test]
    fn add_valid_filter_for_disabled_user() {
        let mut state = State::new();
        state.add_user(EmailRef::new("some@example.com"));
        state.enable(EmailRef::new("some@example.com"), false);

        let res = state.add_filter(EmailRef::new("some@example.com"), ".*some_word.*");
        assert_eq!(res, Err(AddFilterResult::UserDisabled));
        let res = state.enable_filter(EmailRef::new("some@example.com"), true);
        assert_eq!(res, Err(AddFilterResult::UserDisabled));
        let res = state.enable_filter(EmailRef::new("some@example.com"), false);
        assert_eq!(res, Err(AddFilterResult::UserDisabled));
    }

    #[test]
    fn enable_non_configured_filter_for_existing_user() {
        let mut state = State::new();
        state.add_user(EmailRef::new("some@example.com"));

        let res = state.enable_filter(EmailRef::new("some@example.com"), true);
        assert_eq!(res, Err(AddFilterResult::FilterNotConfigured));
        let res = state.enable_filter(EmailRef::new("some@example.com"), false);
        assert_eq!(res, Err(AddFilterResult::FilterNotConfigured));
    }

    #[test]
    fn enable_invalid_filter_for_existing_user() {
        let mut state = State::new();
        state.add_user(EmailRef::new("some@example.com"));
        state.users[0].filter = Some(Filter::new("invlide_filter_set_from_outside["));

        let res = state.enable_filter(EmailRef::new("some@example.com"), true);
        assert_eq!(res, Err(AddFilterResult::InvalidFilter));
        let res = state.enable_filter(EmailRef::new("some@example.com"), false);
        assert_eq!(res, Err(AddFilterResult::InvalidFilter));
    }

}
