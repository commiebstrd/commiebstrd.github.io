---
title: Object Shadowing for Serialization of Complex Types
date: 2018-03-01 00:00:00 -0500
categories: rustlang serde json
future: true
---
Presently, I'm busy writing a capture the flag (CTF) scoreboard, it requires rather complex structures and relationships with other internal objects. Being a security event, I'd also like to maintain explicit control of user data. While serialization in Rust has come a significant way, leveraging auto-generation presents some issues.

* Hard to trust deserialized objects originating from user input.
  * Especially for any extended runtime usage.
* Equally difficult to ensure data leaking to users on serialization.
* `#[serde(...)]` attributes are amazing! Yet have some difficulties themselves.
  * Renaming doesn't always work with your other code ideas
  * It appears multiple attributes may not always work together such as `default` and `with`

Rust has a few concepts that can be used with serialization and resolve most of our issues! If we extend object or variable shadowing from instance usage to a conceptual level surrounding structures, we can also utilize Rust's native conversion functions or implement like-minded specific converters. These alone allow easing of object creation and additional input validation points.

### Code Setup

Suppose we have a scoreboard, it must contain a few basic but related structures that must also be user presentable. Challenges are our most basic object, containing a name or title, flag that a user must submit to score points, and a point value applied when a user solves the challenge.

```rust
struct Challenge {
  name: String,
  flag: String, // secret
  value: usize
  ...
}
```

Players can also be relatively simple holding only a name, reference to team, and list of references to solved challenges. As this will be serving many players and likely needs thread safety for a future Futures implementation, we'll hold references via Arcs.

```rust
type ChallengeList = Vec<Arc<Challenge>>;
struct Player {
  name: String,
  team: Arc<Team>,
  solves: ChallengeList,
  ...
}
```

Finally, Teams also contain a name, and references to member players.
```rust
type PlayerList = Vec<Arc<Player>>;
struct Team {
  name: String,
  players: PlayerList,
  ...
}
```

### Initial De/Serialization

We can imagine that teams and players derive scores from unique solved challenges, allow for players without a team, and a minimal amount of data to transfer between server and users. Serialization could be derived from attribute tags and likely a custom function or two to wrap Arc types. This absolutely works but leaves a bit to be desired when validating user input and massaging objects back into a proper global scope, while maintaining type safety.

```rust
type PlayerList = Vec<Arc<Player>>;
type ChallengeList = Vec<Arc<Challenge>>;

#[derive(Serialize, Deserialize)]
#[serde(default, rename_all="PascalCase")]
struct Challenge {
                               name: String,
  #[serde(skip_serialization)] flag: String, // secret
                               value: usize,
  ...
}
#[derive(Serialize, Deserialize)]
#[serde(default, rename_all="PascalCase")]
struct Player {
                                name: String,
  #[serde(with=TeamSerializer)] team: Arc<Team>, // unimplemented
                                solves: ChallengeList,
  ...
}
#[derive(Serialize, Deserialize)]
#[serde(default, rename_all="PascalCase")]
struct Team {
                                    name: String,
  #[serde(with="PlayerSerializer")] players: PlayerList,
  ...
}
```

Alternatively, we could manually implement the [Serialize](https://serde.rs/impl-serialize.html) and [Deserializer](https://serde.rs/impl-deserialize.html) traits. This also works quite well when done properly. It is however, significantly more difficult to conceptualize and understand. Manual serialization to be prone to deserialization breakage without excessive error handling, I choose to leave `try!{}` or `?` instead of handling errors properly.

*Feel free to skim this example, it is not relevant other than complexity.*
```rust
use std::fmt;
use std::sync::Arc;
use serde::ser::{Serialize, Serializer, SerializeStruct};
use serde::de::{self, Deserialize, Deserializer, Visitor, SeqAccess, MapAccess};

use challenge::*;

impl Serialize for Challenge {
  fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where S: Serializer
  {
    let mut state = serializer.serialize_struct("Challenge", 7)?;
    state.serialize_field("name", &self.name)?;
    state.serialize_field("value", &self.value)?;
    state.skip_field("flag")?;
    state.end()
  }
}

impl<'de> Deserialize<'de> for Challenge {
  fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where D: Deserializer<'de>
  {

    enum Field { Name, Summary, Value, Flag, Files, Solvers, Locked };
    impl<'de> Deserialize<'de> for Field {
      fn deserialize<D>(deserializer: D) -> Result<Field, D::Error>
        where D: Deserializer<'de>
      {

        struct FieldVisitor;
        impl<'de> Visitor<'de> for FieldVisitor {
          type Value = Field;

          fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
            formatter.write_str("`Name`, `Value`, `Flag`")
          }
          fn visit_str<E>(self, value: &str) -> Result<Field, E>
            where E: de::Error
          {
            match value {
              "name" => Ok(Field::Name),
              "value" => Ok(Field::Value),
              "flag" => Ok(Field::Flag),
              _ => Err(de::Error::unknown_field(value, FIELDS)),
            }
          } // visit_str
        } // impl visitor
        deserializer.deserialize_identifier(FieldVisitor)
      } // Field::deserialize
    } // imp deserialize for Field

    struct ChallengeVisitor;
    impl<'de> Visitor<'de> for ChallengeVisitor {
      type Value = Challenge;

      fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        formatter.write_str("Challenge name and flag")
      }
      fn visit_seq<V>(self, mut seq: V) -> Result<Challenge, V::Error>
        where V: SeqAccess<'de>
      {
        let name = seq.next_element()?
          .ok_or_else(|| de::Error::invalid_length(0, &self))?;
        let value = seq.next_element()?
          .ok_or_else(|| de::Error::invalid_length(0, &self))?;
        let flag = seq.next_element()?
          .ok_or_else(|| de::Error::invalid_length(0, &self))?;
        Ok(Challenge::default()
          .set_name(name)
          .set_value(value)
          .set_flag(flag)
        )
      } // visit_seq
      fn visit_map<V>(self, mut map: V) -> Result<Challenge, V::Error>
        where V: MapAccess<'de>
      {
        let mut name = None;
        let mut value = None;
        let mut flag: Option<String> = None;
        while let Some(key) = map.next_key()? {
          match key {
            Field::Name => {
              if name.is_some() {
                return Err(de::Error::duplicate_field("Name"));
              }
              name = Some(map.next_value()?);
            },
            Field::Value => {
              if value.is_some() {
                return Err(de::Error::duplicate_field("Value"));
              }
              value = Some(map.next_value()?);
            },
            Field::Flag => {
              if flag.is_some() {
                return Err(de::Error::duplicate_field("Flag"));
              }
              flag = Some(map.next_value()?);
            },
          };
        }
        let name = name.ok_or_else(|| de::Error::missing_field("Name"))?;
        let mut challenge = Challenge::default().set_name(name);
        if let Some(v) = value {
          challenge = challenge.set_value(v);
        }
        if let Some(f) = flag {
          challenge = challenge.set_flag(&f);
        }
        Ok(challenge)
      }
    } // impl Visitor for Challenge Visitor
    const FIELDS: &'static [&'static str] =
      &["name", "value", "flag"];
    deserializer.deserialize_struct("Challenge", FIELDS, ChallengeVisitor)
  }
}
```

### Shadowing Complex Types

We briefly touched on the fact that Arc types cannot be directly deserialized and require manual handling. You might expect Serde or Arcs to provide a unique identifier within global program scope, such that deserialization could find this identifier and instantiate a new Arc to the base object. Unfortunately, this is not the case within the lifetime of a single program execution, much less several, as each arc is newly created per runtime. Additionally, user input should always be considered untrusted, separating objects used for program logic from submitted data provides a clear separation of boundaries and allows Rust's type system to ensure safety. To start, let's add additional data to our base types and remove serialization.

```rust
mod internal {
  type MD5Sum = [u8; 16]; // a small slice will do
  type PlayerList = Vec<Arc<Player>>;
  type ChallengeList = Vec<Arc<Challenge>>;

  pub struct Challenge {
    name: String,
    id: MD5Sum,
    flag: String, // secret
    value: usize,
    ...
  }
  pub struct Player {
    name: String,
    email: String,   // secret
    password: MD5Sum // secret, and don't actually do this!
    id: MD5Sum,
    team: Arc<Team>,
    solves: ChallengeList,
    ...
  }
  pub struct Team {
    name: String,
    id: MD5Sum,
    players: PlayerList,
    ...
  }
}
```

Then we can add external types for Serde to handle. They don't look too different for our example, comparing with the serialized internal types we can see how Serde's attributes allow easy control when needed. This allows for very easy control of data sent to users. By using `skip_serialization` we control what data is sent to a user, and with `skip_deserialization` we indicate what data to ignore and instantiate as a default value. As these are simpler structures, used for immediate needs only, you may wish to `pub` members opposed to deriving or implementing getters and setters.

```rust
mod external {
  type MD5Sum = [u8; 16]; // a small slice will do
  type PlayerList = Vec<MD5Sum>;
  type ChallengeList = Vec<MD5Sum>;

  #[derive(Serialize, Deserialize)]
  #[serde(default, rename_all="PascalCase")]
  pub struct Challenge {
                                   pub name: String,
                                   pub id: MD5Sum,
    #[serde(skip_serialization)]   pub flag: String, // secret
    #[serde(skip_deserialization)] pub value: usize,
    ...
  }
  #[derive(Serialize, Deserialize)]
  #[serde(default, rename_all="PascalCase")]
  pub struct Player {
                                                     pub name: String,
                                                     pub id: MD5Sum,
    #[serde(skip_serialization_if="isSamePlayer")]   pub email: String,   // secret, if not this player
    #[serde(skip_serialization)]                     pub password: MD5Sum
                                                     pub team: MD5Sum,
    #[serde(skip_deserialization)]                   pub solves: ChallengeList,
    ...
  }
  #[derive(Serialize, Deserialize)]
  #[serde(default, rename_all="PascalCase")]
  pub struct Team {
                                   pub name: String,
                                   pub id: MD5Sum,
    #[serde(skip_deserialization)] pub players: PlayerList,
    ...
  }
}
```

### Using Conversions as Safety Boundaries

The final pieces we need are the ability to convert between `internal` and `external` variants while controlling and validating flow of data. Conversions could be controlled with `From` or `Into` traits only, however this makes error transitions and conversion from Uid to Arc, with user input tricky. Instead, leveraging native `Into` for conversions from `internal` to `external` and creating custom composers from `external` to `internal` types on a global scope or higher-level object managing these structures, works very well. One drawback Rust allows, by default `From` and `Into` traits also create the inverse, meaning you could convert an `external` type to `internal` via those traits and aside from potential runtime failures, Rust should not complain during compilation. I do not know of a way to prevent this additional logic, as such special care must be taken not to use inverse functions.

```rust
mod internal {
  use external::Challenge as ExtChallenge;
  use external::Player as ExtPlayer;
  use external::Team as ExtTeam;

  impl Into<ExtChallenge> for Challenge {
    fn into(self) -> ExtChallenge {
      ExtChallenge {
        name: self.name,
        id: self.id,
        flag: "".to_string(), // never send to external objects
        value: self.value,
      }
    }
  }
  impl Into<ExtPlayer> for Player {
    fn into(self) -> ExtPlayer {
      ExtPlayer {
        name: self.name,
        email: self.email, // control release through serde
        password: [0; 16], // Never send to ext objects
        team: self.team.get_id(),
        solves: self.solves.iter().map(|s| s.get_id()).collect(),
      }
    }
  }
  impl Into<ExtTeam> for Team {
    fn into(self) -> ExtTeam {
      ExtTeam {
        name: self.name,
        id: self.id,
        players: self.players.iter().map(|p| p.get_id()).collect(),
      }
    }
  }
}
```
*Assigning member values to `Ext` variants directly would not be possible without `pub` or placing conversion in `external`*

Unlike converting from `internal` to `external` which we know *must* succeed and be allowed, user input and validation gives us no such guarantees. If your application/library supports global scoping or runtimes such as Futures aims to, I'd imagine this could be handled through a much simpler global module for deserialization that verifies and manipulates input to fit our needs. For now, we'll consider a simplistic engine structure, managing our various objects and providing needed safe conversions.

```rust
use internal::{Challenge, Player, Team};
use external::MD5Sum;
use external::Challenge as ExtChallenge;
use external::ChallengeList as ExtChallengeList;
use external::Player as ExtPlayer;
use external::PlayerList as ExtPlayerList;
use external::Team as ExtTeam;

type ChallengeList = Vec<Challenge>;
type PlayerList = Vec<Player>;
type TeamList = Vec<Team>;
type EngineResult = Result<T, EngineError>;
type ChallengeResult = EngineResult<Challenge>;
type PlayerResult = EngineResult<Player>;
type TeamResult = EngineResult<Team>;

pub struct Engine { // box allocated, most likely
  challenges: ChallengeList,
  players: PlayerList,
  teams: TeamList,
}
impl Engine {
  ...
  pub fn from_ext_challenge(&self, chal: &ExtChallenge) -> ChallengeResult {
    // we don't care about the name, feel free to submit incorrectly. ID and flag, must match only
    let id = chal.valid_id()?;            // valid_member for conversion, `?` convert to EngineError
    let flag = chal.valid_flag()?;
    // value is skipped, find/validate via internal copy
    let internal_chal = self.challenges.iter().fold(
        Challenge::default() |mut acc, c| // internal default with None/empty members
          if c.get_id() == id {
            acc = c;                      // internal::Challenge::default
            acc.set_flag(flag);           // set to user controlled flag
          }
    });
    // validate challenge as needed
    Ok(internal_chal)
  }
  pub fn from_ext_player(&self, player: ExtPlayer) -> PlayerResult {
    // validate base objects for accepting this player
    let name = player.valid_name()?;
    let email = player.valid_email()?;
    let pass = player.valid_pass()?;
    let internal_player = self.players.iter().fold(
      Err(PlayerError::NoMatch), |acc, p| {
        if p.name() == name && p.email() == email {
          return Ok(p);
        }
        acc
    });
    match internal_player {
      Err(pe) => pe.into(), // native conversion to EngineError
      Ok(p) => if p.check_password(pass) {
                Ok(p) // return internal object with nothing from user input
              } else {
                Err(EngineError::InvalidPassword)
              }
    }
  }
}
```
*For our usage, we'll consider Engine conversions thread local to any one conversion, otherwise objects are shared directly or boxed at least.*

### Validating User Input from Rocket

At this point we can safely transition an `internal` object with potentially secret data to `external` of the same type. We can also take user input and safely test against existing objects. You might use this for input/output from web applications, databases, or config files. Below is a simple example using [Rocket](https://rocket.rs) for user login with our engine held in a `State<RwLock<>>`. Holding Rocket managed objects in RwLock allows easy internal mutability.


```rust
#![feature(plugin)]
#![plugin(rocket_codegen)]

extern crate rocket;
extern crate rocket_contrib;

use std::sync::RwLock;
use rocket::State;
use rocket_contrib::Json;
use internal::Player;
use external::Player as ExtPlayer;

type RwEngine = State<RwLock<Engine>>;

#[get("/login" format="application/json", data="<player>")]
fn login(player: Json<ExtPlayer>, engine: RwEngine) -> &'static str {
  let eplayer = player.into_inner(); // get ExtPlayer
  let engine = engine.inner().read()?;
  match engine.from_ext_player(eplayer) {
    Err(e) => e.to_str(),
    Ok(p) => format!{"Correctly logged in as: {}", p.name()}, // p is now Player
  }
}

fn main() {
    let engine = RwLock::new(Engine::default());

    rocket::ignite()
    .manage(engine)
    .mount("/", routes![login])
    .launch();
}
```

### Conclusion

We stepped through several means of serializing and deserializing types with Serde. It's nearly impossible for them to expect every use case another programmer might employ against their library, they do a heck of a job as it is!Hopefully you find this technique user friendly for serializing complex types, allowing focus back on difficult tasks instead of data input/output. I find it fit's more comfortably with my understanding and expectations from Rust and Serde. If you've found alternative was of achieving the same idea, please share!