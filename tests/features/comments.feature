Feature: review comments

  The whole point of the bot: get comments when changes get review comments.

  Background:
    Given a person named Alice Smith with email address alice@bloom.com
      And a person named Bob Jones with email address bob@jones.com
      And a bot named Reviewbot
      And everybody sends the enable command to the bot
      And a Gerrit project named tools
  
  Scenario: review without message
     Given Bob uploads a new change to the tools project
       And Alice replies to Bob's change with Code-Review+2
      When we check for messages by the bot
      Then there is a message for Bob which includes the text "Code-Review"
       And this message includes the text "+2"
       And this message includes the text "Submittable"

  Scenario: review with message
     Given Bob uploads a new change to the tools project
       And Alice replies to Bob's change with Code-Review+2 and the comment "Good job!"
      When we check for messages by the bot
      Then there is a message for Bob which includes the text "Code-Review"
       And this message includes the text "+2"
       And this message includes the text "Good job!"
       And this message includes the text "Submittable"

  Scenario: insufficient review without message
     Given Bob uploads a new change to the tools project
       And Alice replies to Bob's change with Code-Review+1
      When we check for messages by the bot
      Then there is a message for Bob which includes the text "Code-Review"
       And this message includes the text "+1"
       And this message does not include the text "Submittable"

  Scenario: insufficient review without message
     Given Bob uploads a new change to the tools project
       And Alice replies to Bob's change with Code-Review+1 and the comment "Okay job."
      When we check for messages by the bot
      Then there is a message for Bob which includes the text "Code-Review"
       And this message includes the text "+1"
       And this message does not include the text "Submittable"

  Scenario: inline comments
     Given Bob uploads a new change to the tools project
       And Bob creates the file "README" with the following content in the change:
       """
       Lorem ipsum dolor sit amet, consectetur adipiscing elit. Curabitur
       volutpat ornare convallis. Sed luctus imperdiet nisl, at malesuada risus
       tincidunt vitae. Donec iaculis, lectus ac tempor ullamcorper, lorem odio
       eleifend quam, id ullamcorper metus lacus quis lorem. Aenean fringilla,
       erat vitae rhoncus ultrices, purus est pharetra erat, ac auctor nunc
       sapien eget justo. Nam in iaculis lacus. Cras sit amet nibh libero. Etiam
       non sem quis tortor efficitur ornare sit amet a eros. Aenean commodo
       tempor lectus, id fermentum nisl semper nec. Mauris quis odio sit amet
       nulla volutpat porta et et turpis.
       """
       And Alice replies to Bob's change with Code-Review-2 and the comment "Boo!" and the following inline comments:
       """
       File: README
       Line 0: You shouldn't be adding this file in the first place.
       Line 8: Who even is Mauris?
       """
      When we check for messages by the bot
      Then there is a message for Bob which includes the text "Code-Review"
       And this message includes the text "-2"
       And this message includes the text "Boo!"
       And this message includes the text "README"
       And this message includes the text "You shouldn't be adding this file in the first place."
       And this message includes the text "Who even is Mauris?"

  Scenario: inline comments without approval
     Given Bob uploads a new change to the tools project
       And Bob creates the file "README" with the following content in the change:
       """
       We should have a README.
       """
       And Alice replies to Bob's change with the following inline comments:
       """
       File: README
       Line 1: Yes, but please write something useful at least.
       """
      When we check for messages by the bot
      Then there is no message for Bob which includes the text "Code-Review"
       And there is a message for Bob which includes the text "Yes, but please write something useful at least."

  Scenario: comment without approval is not sent by default
     Given Bob uploads a new change to the tools project
       And Alice replies to Bob's change with the comment "I don't care."
      When we check for messages by the bot
      Then there is no message for Bob which includes the text "I don't care."

  Scenario: comment without approval can be enabled
     Given Bob sends the enable notify_review_comments command to the bot
       And Bob uploads a new change to the tools project
       And Alice replies to Bob's change with the comment "I don't care."
      When we check for messages by the bot
      Then there is a message for Bob which includes the text "I don't care."

  Scenario: comment from a bot gets special formatting
     Given Bob uploads a new change to the tools project
       And Reviewbot replies to Bob's change with Code-Review-2 and the following comment:
       """
       Acquiring the funds: SUCCESS
       Executing the plans: FAILURE
       """
      When we check for messages by the bot
      Then there is a message for Bob which includes the text "plans"
       And this message does not include the text "funds"
