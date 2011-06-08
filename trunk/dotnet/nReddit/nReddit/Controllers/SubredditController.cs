using System;
using System.Collections.Generic;
using System.Data;
using System.Data.Entity;
using System.Linq;
using System.Web;
using System.Web.Mvc;
using nReddit.Models;

namespace nReddit.Controllers
{ 
    public class SubredditController : Controller
    {
        private NRedditEntities db = new NRedditEntities();

        //
        // GET: /Subreddit/

        public ViewResult Index()
        {
            return View(db.Subreddits.Include(s => s.Submissions).OrderByDescending(
                x => x.Submissions.Count).ToList());
        }

        //
        // GET: /Subreddit/Details/5

        public ViewResult Details(int id)
        {
            Subreddit subreddit =
                db.Subreddits.Include(s => s.Submissions).Single(s => s.SubredditID == id);
            Session["SubredditID"] = subreddit.SubredditID;
            return View(subreddit);
        }

        //
        // GET: /Subreddit/Create

        public ActionResult Create()
        {
            return View();
        } 

        //
        // POST: /Subreddit/Create

        [HttpPost]
        public ActionResult Create(Subreddit subreddit)
        {
            if (ModelState.IsValid)
            {
                db.Subreddits.Add(subreddit);
                db.SaveChanges();
                return RedirectToAction("Index");  
            }

            return View(subreddit);
        }
        
        //
        // GET: /Subreddit/Edit/5
 
        public ActionResult Edit(int id)
        {
            Subreddit subreddit = db.Subreddits.Find(id);
            return View(subreddit);
        }

        //
        // POST: /Subreddit/Edit/5

        [HttpPost]
        public ActionResult Edit(Subreddit subreddit)
        {
            if (ModelState.IsValid)
            {
                db.Entry(subreddit).State = EntityState.Modified;
                db.SaveChanges();
                return RedirectToAction("Index");
            }
            return View(subreddit);
        }

        //
        // GET: /Subreddit/Delete/5
 
        public ActionResult Delete(int id)
        {
            Subreddit subreddit = db.Subreddits.Find(id);
            return View(subreddit);
        }

        //
        // POST: /Subreddit/Delete/5

        [HttpPost, ActionName("Delete")]
        public ActionResult DeleteConfirmed(int id)
        {            
            Subreddit subreddit = db.Subreddits.Find(id);
            db.Subreddits.Remove(subreddit);
            db.SaveChanges();
            return RedirectToAction("Index");
        }

        protected override void Dispose(bool disposing)
        {
            db.Dispose();
            base.Dispose(disposing);
        }
    }
}