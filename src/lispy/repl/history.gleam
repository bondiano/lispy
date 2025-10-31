import gleam/erlang/process
import gleam/list
import gleam/otp/actor
import gleam/string
import simplifile

const history_file = ".lispy_history"

const max_history = 1000

pub type HistoryActor =
  process.Subject(Message)

type State {
  State(entries: List(String))
}

pub type Message {
  Add(entry: String)
  Get(reply: process.Subject(List(String)))
  Save(reply: process.Subject(Result(Nil, String)))
}

pub fn start() -> Result(HistoryActor, actor.StartError) {
  let initial_entries = load_from_file()

  case
    actor.new(State(entries: initial_entries))
    |> actor.on_message(handle_message)
    |> actor.start
  {
    Ok(started) -> Ok(started.data)
    Error(err) -> Error(err)
  }
}

fn handle_message(state: State, message: Message) -> actor.Next(State, Message) {
  case message {
    Add(entry) -> {
      let trimmed = string.trim(entry)
      case trimmed {
        "" -> actor.continue(state)
        _ -> {
          let new_entries = add_entry(trimmed, state.entries)
          actor.continue(State(entries: new_entries))
        }
      }
    }

    Get(reply) -> {
      process.send(reply, list.reverse(state.entries))
      actor.continue(state)
    }

    Save(reply) -> {
      let result = save_to_file(state.entries)
      let reply_msg = case result {
        Ok(_) -> Ok(Nil)
        Error(_) -> Error("Failed to save history")
      }
      process.send(reply, reply_msg)
      actor.continue(state)
    }
  }
}

fn add_entry(entry: String, entries: List(String)) -> List(String) {
  case entries {
    [last, ..] if last == entry -> entries
    _ -> {
      let new_entries = [entry, ..entries]
      case list.length(new_entries) > max_history {
        True -> list.take(new_entries, max_history)
        False -> new_entries
      }
    }
  }
}

pub fn add(history_actor: HistoryActor, entry: String) -> Nil {
  process.send(history_actor, Add(entry))
}

pub fn get(history_actor: HistoryActor) -> List(String) {
  actor.call(history_actor, 100, Get)
}

pub fn save(history_actor: HistoryActor) -> Result(Nil, String) {
  actor.call(history_actor, 1000, Save)
}

fn load_from_file() -> List(String) {
  case simplifile.read(history_file) {
    Ok(content) -> {
      content
      |> string.split("\n")
      |> list.filter(fn(line) { string.trim(line) != "" })
      |> list.reverse
    }
    Error(_) -> []
  }
}

fn save_to_file(entries: List(String)) -> Result(Nil, simplifile.FileError) {
  let content =
    entries
    |> list.reverse
    |> string.join("\n")

  simplifile.write(history_file, content)
}
