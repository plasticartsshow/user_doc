#![allow(unused_imports)]
#![allow(dead_code)]
use user_doc::*;

#[test]
fn must_extract_doc_comments () {
  #[user_doc_fn(
    chapter_num_slug(1, 3, 5),
    chapter_name_slug(
      "A Slaying in Luton", 
      "The Trouble About Ipswich",
      "All Along the Weary M-5 Motorway",
    ),
  )]
  /// The parenchyma isn't as stiff as usual. It looks almost floppy.
  /// I stick out a hand to touch it. It sucks my fingertips forward.
  /// When I pull my hand back, a hanging bridge of sap follows. Amazing. I've only ever seen it dry.
  pub fn call_this_function() -> bool { true }
  
  #[derive(user_doc_item)]
  /// This block of text must not be captured. There is no way to associate it with a chapter or chapter path.
  /// "Stand up slow. Don't make a mess of things." 
  /// He said it with a sneer. The expression was set in stone – like he didn't know how to make any others.
  /// I had never seen him before. I did recognize what he was carrying though – The long hook was impossible to mistake. He was a people-taker.
  /// I stood up. 
  /// "Good" he said, the sneer never changing. "Now over there with the others."
  /// Others? I looked toward where he was gesturing with the hook. A ragged line of people stood by the wall. 
  /// This block of text must not be captured. There is no way to associate it with a chapter or chapter path.
  pub struct Dummy {
    #[chapter_num(11)]
    /// "So let them come to see their ruler." He spoke as if he didn't care. He took great pains to do so with a loud voice.  
    /// Maybe it was because he still had one headphone on. I could not tell.   
    /// "They won't breach our guard so easily, right fellas?" Nobody answered. Maybe he was trying to impress them.   
    /// If that was it, it wasn't working.  
    pub ding_dong: u8,
    #[chapter_name("The Peace Unmade")]
    #[chapter_num(33)]
    /// They wrecked the craft up on the higher shore.  
    /// They weren't pressed for space. There was plenty of shore to wreck upon, too.  
    /// The Great Receding had left lots of sand where once the mighty waves had smashed.  
    /// Its passage revealed even more garbage. Sometimes, you could find something good there. You had to be careful though.  
    /// Some of the garbage was dangerous. It was best to wrap cloth around your hands.  
    /// The wrecked craft left a kind of streak through the junk – Like a giant hand had decided to clean some of the mess before getting tired and quitting abruptly.
    pub ping_pong: u8,
    #[chapter_name_slug(
      "A Slaying in Luton", 
      "The Trouble About Ipswich",
      "Their Modest Abode Ruined",
    )]
    #[chapter_num_slug(1,3,1)]
    /// "Can I have a drink of that water?" she said.  
    /// It was hot. The cool liquid ran freely behind me.  
    /// "Why not share it?" I thought. I was leaning on the fence to relax. I hadn't meant to block it.  
    /// "Go ahead." I stepped aside. She passed through the fence gate.  
    /// It's not actually mine, I told her." I didn't want to say anything. I wanted to lean back on the gate. I didn't because I didn't feel like moving out of someone's way again. It was hot.  
    /// "What, all this water is just here like this?" She seemed doubtful.  
    /// "Yeah," I said, as if the expert, "I think it belongs to the city."  
    /// "How strange," she said.  
    /// It was that kind of conversation. Every word. Took too much. Breath. It was hot.
    pub bing_bong: u8,
  }

  #[derive(user_doc_item, Clone, Debug, PartialEq, Eq)]
  /// This comment must not be captured for user docs.
  pub enum Idiot {
    #[chapter_num_slug(1, 3, 6)]
    #[chapter_blurb("- Read about the Kid in 1.3.6")]
    #[chapter_name("Turtles, Terrapins, and Toroidal Tortoises – Or, Brixton Bereaved")]
    /// He took a look at the card I showed him. His brows scrunched up like he smelled something offensive.  
    /// Could he tell it was fake? Everyone sweats in heat like this. If they don't, it means they're about to keel over from dehydration anyway. Still, I felt like I was sweating more than usual.  
    /// We stood frozen for a moment. It seemed an eon. Then he mercifully broke the silence.   
    /// "I can't read those. I'm just supposed to stand here and not let anyone pass."  
    /// I considered. I didn't want to get him in trouble – he was clearly new. Still, it was me or him.  
    /// "Oh," I said as casually as I could, "It says you're to let me through. But don't let anyone else through after me. It says that too.""  
    Kid(u16),
    #[chapter_num(23)] // should be overriden by slug 
    #[chapter_num_slug(1, 3, 4)]
    #[chapter_name("The House of Almond Blossoms")]
    #[chapter_blurb("- Read about the HeroLady in 1.3.4")]
    /// The machine came barelling down the street. It was a behemoth of piping and tubes.  
    /// The device was a wonder as I had never seen. A woman sat on it. She wore a fearsome helmet of obviously fine make.   
    /// Every time she kicked her feet, the strange thing seemed to jump forward. Was she hurting the beast to goad it on? Was it even alive?  
    /// I did not know. A crowd had come to see. I did not notice. I was alone among them with my thoughts. Would this be my end? Would I be trod into the earth by a mechanical nightmare?  
    HeroLady(u64),
    /// This comment must not be captured for user docs 
    /// "I am a cruel governess," she bellowed, spittle flying. Rarely in my life had I occasioned to soil myself. I chose to do so now. 
    /// This comment must not be captured for user docs 
    TyrannicalGoverness(u128)
  }
  // 
  let i = Idiot::Kid(0);
  let p = Idiot::HeroLady(0);
  assert_ne!(
    p, i,
    "Other non-'user_doc' derives must work as intended without interference"
  );
  // 
  user_doc::load_global_docs(None, None).expect("must load global docs into runtime");
  let docs = &*user_doc::DOCS;
  let docs_read_lock = docs.read().expect("must get read guard on global docs");
  
  /*
    Structure in memory at runtime 
  */ 
  std::println!("docs_read_lock {:?} {:#?}", std::time::Instant::now(), *docs_read_lock );
  assert_eq!(
    docs_read_lock.get_entry_at_numeric_path(&[1,3,4]).expect("must find test entry").0,
    String::from("The House of Almond Blossoms"),
    "It must have inserted the commented chapter at the given position with the numeric path overriding a single chapter number"
  );
  assert_eq!(
    docs_read_lock.get_entry_at_numeric_path(&[33]).expect("must find test entry").0,
    String::from("The Peace Unmade"),
    "It must have inserted the commented chapter at the given position at the root-level path"
  );
  let parenchyma_target = " The parenchyma isn\\'t as stiff as usual. It looks almost floppy.\
    \n I stick out a hand to touch it. It sucks my fingertips forward.\
    \n When I pull my hand back, a hanging bridge of sap follows. Amazing. I\\'ve only ever seen it dry.";
  assert_eq!(
    docs_read_lock.get_entry_at_numeric_path(&[1,3,5]).expect("must find test entry").1,
    parenchyma_target,
    "It must insert the comment text at the given position with line breaks"
  );
  
  /* 
    Structure expanded to markdown dirs at runtime 
  */
  docs_read_lock.expand_into_mdbook_dirs_at_path(
    DirectoryNamingScheme::ChapterName,
    "tests/scratch/src",
  ).expect("must expand docs into dirs ");
  let mut target_path = std::path::PathBuf::new();
  target_path.push("tests");
  target_path.push("scratch");
  target_path.push("src");
  target_path.push("1 - A Slaying in Luton");
  target_path.push("3 - The Trouble About Ipswich");
  target_path.push("5 - All Along the Weary M-5 Motorway");
  target_path.set_extension("md");
  let file_contents = std::fs::read(
    target_path
  ).expect("must open target markdown file in  dirs");
  let parenchyma_file_target = format!(
    "# 5 - All Along the Weary M-5 Motorway  \n{}", parenchyma_target
  );  
  assert_eq!(
    String::from_utf8(file_contents).expect("must parse file contents to String"),
    parenchyma_file_target,
    "Must expand content to markdown file in dirs."
  );
}
