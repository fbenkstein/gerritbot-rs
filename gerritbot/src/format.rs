use rlua::{
    FromLua, Function as LuaFunction, Lua, MultiValue as LuaMultiValue, StdLib as LuaStdLib,
    Value as LuaValue,
};
use serde::Serialize;

use gerritbot_gerrit as gerrit;

use crate::state::{User, NOTIFICATION_FLAGS};

pub const DEFAULT_FORMAT_SCRIPT: &str = include_str!("format.lua");
const LUA_FORMAT_COMMENT_ADDED: &str = "format_comment_added";
const LUA_FORMAT_REVIEWER_ADDED: &str = "format_reviewer_added";
const LUA_FORMAT_FUNCTIONS: &[&str] = &[LUA_FORMAT_COMMENT_ADDED, LUA_FORMAT_REVIEWER_ADDED];

pub struct Formatter {
    lua: Lua,
}

impl Default for Formatter {
    fn default() -> Self {
        Self {
            lua: load_format_script(DEFAULT_FORMAT_SCRIPT).unwrap(),
        }
    }
}

fn load_format_script(script_source: &str) -> Result<Lua, String> {
    let lua_std_lib = LuaStdLib::BASE | LuaStdLib::STRING | LuaStdLib::TABLE;
    let lua = Lua::new_with(lua_std_lib);
    lua.context(|context| -> Result<(), String> {
        let globals = context.globals();
        context
            .load(script_source)
            .exec()
            .map_err(|err| format!("syntax error: {}", err))?;

        // check that the required functions are present
        for function_name in LUA_FORMAT_FUNCTIONS {
            let _: LuaFunction = globals
                .get(*function_name)
                .map_err(|_| format!("{} function missing", function_name))?;
        }

        Ok(())
    })?;
    Ok(lua)
}

fn get_flags_table<'lua>(user: &User, lua: rlua::Context<'lua>) -> rlua::Result<rlua::Table<'lua>> {
    lua.create_table_from(NOTIFICATION_FLAGS.iter().cloned().filter_map(|flag| {
        if user.has_flag(flag) {
            Some((flag.to_string(), true))
        } else {
            None
        }
    }))
}

impl Formatter {
    pub fn new(format_script: &str) -> Result<Self, String> {
        Ok(Self {
            lua: load_format_script(&format_script)?,
        })
    }

    fn format_lua<'lua, T, E>(
        lua: rlua::Context<'lua>,
        function_name: &str,
        user: &User,
        event: E,
        extra_args: Vec<LuaValue<'lua>>,
    ) -> Result<T, String>
    where
        T: FromLua<'lua>,
        E: Serialize,
    {
        let globals = lua.globals();

        let format_function: LuaFunction = globals
            .get(function_name)
            .map_err(|_| format!("{} function missing", function_name))?;

        let event = rlua_serde::to_value(lua, event)
            .map_err(|e| format!("failed to serialize event: {}", e))?;
        let flags = get_flags_table(user, lua)
            .map(LuaValue::Table)
            .map_err(|err| format!("failed to create flags table: {}", err))?;

        let args = {
            let mut v = Vec::with_capacity(2 + extra_args.len());
            v.push(event);
            v.push(flags);
            let mut extra_args = extra_args;
            v.append(&mut extra_args);
            LuaMultiValue::from_vec(v)
        };

        let result = format_function
            .call::<_, LuaValue>(args)
            .map_err(|err| format!("lua formatting function failed: {}", err))?;

        T::from_lua(result, lua).map_err(|e| format!("failed to convert formatting result: {}", e))
    }

    pub fn format_comment_added(
        &self,
        user: &User,
        event: &gerrit::CommentAddedEvent,
        is_human: bool,
    ) -> Result<Option<String>, String> {
        self.lua.context(|context| {
            Formatter::format_lua(
                context,
                LUA_FORMAT_COMMENT_ADDED,
                user,
                event,
                vec![LuaValue::Boolean(is_human)],
            )
        })
    }

    pub fn format_reviewer_added(
        &self,
        user: &User,
        event: &gerrit::ReviewerAddedEvent,
    ) -> Result<String, String> {
        self.lua.context(|context| {
            Formatter::format_lua(context, LUA_FORMAT_REVIEWER_ADDED, user, event, vec![])
        })
    }

    pub fn format_status(
        &self,
        user: Option<&User>,
        enabled_user_count: usize,
    ) -> Result<String, String> {
        let enabled = user
            .map(|u| u.has_any_flag(NOTIFICATION_FLAGS))
            .unwrap_or(false);
        let enabled_user_count = enabled_user_count - (enabled as usize);

        Ok(format!(
            "Notifications for you are **{}**. I am notifying {}.",
            if enabled { "enabled" } else { "disabled" },
            match (enabled, enabled_user_count) {
                (false, 0) => format!("no users"),
                (true, 0) => format!("no other users"),
                (false, 1) => format!("one user"),
                (true, 1) => format!("another user"),
                (false, _) => format!("{} users", enabled_user_count),
                (true, _) => format!("another {} users", enabled_user_count),
            }
        ))
    }
}

#[cfg(test)]
mod test {
    use lazy_static::lazy_static;

    use gerritbot_spark as spark;

    use crate::state::State;

    use super::*;

    const EVENT_JSON : &'static str = r#"
{"author":{"name":"Approver","username":"approver","email":"approver@approvers.com"},"approvals":[{"type":"Code-Review","description":"Code-Review","value":"2","oldValue":"-1"}],"comment":"Patch Set 1: Code-Review+2\n\nJust a buggy script. FAILURE\n\nAnd more problems. FAILURE","patchSet":{"number":1,"revision":"49a65998c02eda928559f2d0b586c20bc8e37b10","parents":["fb1909b4eda306985d2bbce769310e5a50a98cf5"],"ref":"refs/changes/42/42/1","uploader":{"name":"Author","email":"author@example.com","username":"Author"},"createdOn":1494165142,"author":{"name":"Author","email":"author@example.com","username":"Author"},"isDraft":false,"kind":"REWORK","sizeInsertions":0,"sizeDeletions":0},"change":{"project":"demo-project","branch":"master","id":"Ic160fa37fca005fec17a2434aadf0d9dcfbb7b14","number":49,"subject":"Some review.","owner":{"name":"Author","email":"author@example.com","username":"author"},"url":"http://localhost/42","commitMessage":"Some review.\n\nChange-Id: Ic160fa37fca005fec17a2434aadf0d9dcfbb7b14\n","status":"NEW"},"project":"demo-project","refName":"refs/heads/master","changeKey":{"id":"Ic160fa37fca005fec17a2434aadf0d9dcfbb7b14"},"type":"comment-added","eventCreatedOn":1499190282}"#;

    const CHANGE_JSON_WITH_COMMENTS : &'static str = r#"
{"project":"gerritbot-rs","branch":"master","id":"If70442f674c595a59f3e44280570e760ba3584c4","number":1,"subject":"Bump version to 0.6.0","owner":{"name":"Administrator","email":"admin@example.com","username":"admin"},"url":"http://localhost:8080/1","commitMessage":"Bump version to 0.6.0\n\nChange-Id: If70442f674c595a59f3e44280570e760ba3584c4\n","createdOn":1524584729,"lastUpdated":1524584975,"open":true,"status":"NEW","comments":[{"timestamp":1524584729,"reviewer":{"name":"Administrator","email":"admin@example.com","username":"admin"},"message":"Uploaded patch set 1."},{"timestamp":1524584975,"reviewer":{"name":"jdoe","email":"john.doe@localhost","username":"jdoe"},"message":"Patch Set 1:\n\n(1 comment)"}]}"#;

    const PATCHSET_JSON_WITH_COMMENTS : &'static str = r#"{"number":1,"revision":"3f58af760fc1e39fcc4a85b8ab6a6be032cf2ae2","parents":["578bc1e684098d2ac597e030442c3472f15ac3ad"],"ref":"refs/changes/01/1/1","uploader":{"name":"Administrator","email":"admin@example.com","username":"admin"},"createdOn":1524584729,"author":{"name":"jdoe","email":"jdoe@example.com","username":""},"isDraft":false,"kind":"REWORK","comments":[{"file":"/COMMIT_MSG","line":1,"reviewer":{"name":"jdoe","email":"john.doe@localhost","username":"jdoe"},"message":"This is a multiline\ncomment\non some change."}],"sizeInsertions":2,"sizeDeletions":-2}"#;

    fn get_event() -> gerrit::CommentAddedEvent {
        let event: Result<gerrit::Event, _> = serde_json::from_str(EVENT_JSON);
        match event.expect("failed to decode event") {
            gerrit::Event::CommentAdded(event) => event,
            event => panic!("wrong type of event: {:?}", event),
        }
    }

    fn get_change_with_comments() -> (gerrit::Change, gerrit::Patchset) {
        let change: Result<gerrit::Change, _> = serde_json::from_str(CHANGE_JSON_WITH_COMMENTS);
        assert!(change.is_ok());
        let patchset: Result<gerrit::Patchset, _> =
            serde_json::from_str(PATCHSET_JSON_WITH_COMMENTS);
        assert!(patchset.is_ok());
        (change.unwrap(), patchset.unwrap())
    }

    lazy_static! {
        static ref FORMAT_TEST_STATE: State = {
            let mut state = State::new();
            state.add_user(spark::EmailRef::new("some@example.com"));
            state
        };
        static ref FORMAT_TEST_USER: &'static User = FORMAT_TEST_STATE
            .find_user(spark::EmailRef::new("some@example.com"))
            .unwrap();
    }

    #[test]
    fn test_format_approval() {
        let event = get_event();
        let res = Formatter::default().format_comment_added(&FORMAT_TEST_USER, &event, true);
        // Result<Option<String>, _> -> Result<Option<&str>, _>
        let res = res.as_ref().map(|o| o.as_ref().map(String::as_str));
        assert_eq!(
            res,
            Ok(Some("[Some review.](http://localhost/42) ([demo-project](http://localhost/q/project:demo-project+status:open)) 👍 +2 (Code-Review) from [Approver](http://localhost/q/reviewer:approver@approvers.com+status:open)\n\n> Just a buggy script. FAILURE\n\n> And more problems. FAILURE"))
        );
    }

    #[test]
    fn format_approval_unknown_labels() {
        let mut event = get_event();
        event
            .approvals
            .as_mut()
            .map(|approvals| approvals[0].approval_type = String::from("Some-New-Type"));
        let res = Formatter::default().format_comment_added(&FORMAT_TEST_USER, &event, true);
        // Result<Option<String>, _> -> Result<Option<&str>, _>
        let res = res.as_ref().map(|o| o.as_ref().map(String::as_str));
        assert_eq!(
            res,
            Ok(Some("[Some review.](http://localhost/42) ([demo-project](http://localhost/q/project:demo-project+status:open)) 🤩 +2 (Some-New-Type) from [Approver](http://localhost/q/reviewer:approver@approvers.com+status:open)\n\n> Just a buggy script. FAILURE\n\n> And more problems. FAILURE"))
        );
    }

    #[test]
    fn format_approval_multiple_labels() {
        let mut event = get_event();
        event.approvals.as_mut().map(|approvals| {
            approvals.push(gerrit::Approval {
                approval_type: "Verified".to_string(),
                description: "Verified".to_string(),
                value: "1".to_string(),
                old_value: None,
                by: None,
            })
        });
        let res = Formatter::default().format_comment_added(&FORMAT_TEST_USER, &event, true);
        // Result<Option<String>, _> -> Result<Option<&str>, _>
        let res = res.as_ref().map(|o| o.as_ref().map(String::as_str));
        assert_eq!(
            res,
            Ok(Some("[Some review.](http://localhost/42) ([demo-project](http://localhost/q/project:demo-project+status:open)) 👍 +2 (Code-Review), 🌞 +1 (Verified) from [Approver](http://localhost/q/reviewer:approver@approvers.com+status:open)\n\n> Just a buggy script. FAILURE\n\n> And more problems. FAILURE"))
        );
    }

    #[test]
    fn format_approval_no_approvals() {
        let mut event = get_event();
        event.approvals = None;
        let res = Formatter::default().format_comment_added(&FORMAT_TEST_USER, &event, true);
        // Result<Option<String>, _> -> Result<Option<&str>, _>
        let res = res.as_ref().map(|o| o.as_ref().map(String::as_str));
        assert_eq!(res, Ok(None));
    }

    #[test]
    fn test_format_comments() {
        let mut event = get_event();
        let (change, patchset) = get_change_with_comments();
        event.comment = "(1 comment)".to_string();
        event.change = change;
        event.patchset = patchset;

        let res = Formatter::default()
            .format_comment_added(&FORMAT_TEST_USER, &event, true)
            .expect("format failed")
            .expect("no comments");

        assert!(res.ends_with("`/COMMIT_MSG`\n\n> [Line 1](http://localhost:8080/#/c/1/1//COMMIT_MSG@1) by [jdoe](http://localhost:8080/q/reviewer:john.doe@localhost+status:open): This is a multiline\n> comment\n> on some change.\n"), "no inline comments: {:?}", res);
    }
}
