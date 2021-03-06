//
//  AppDelegate.swift
//  ios-StormCloud
//
//  Created by Jack Thias on 11/5/17.
//  Copyright © 2017 Jack Thias. All rights reserved.
//

import UIKit
import GoogleSignIn

@UIApplicationMain
class MyAppDelegate: UIResponder, UIApplicationDelegate, GIDSignInDelegate {
    static var idToken : String! = "L33tH4CK3R1DT0K3N"
    
    func sign( signIn: GIDSignIn!, didDisconnectWith user: GIDGoogleUser!, withError error: Error!) {
        NotificationCenter.default.post(
            name: Notification.Name(rawValue: "ToggleAuthUINotification"),
            object: nil,
            userInfo: ["statusText": "User has disconnected."])
    }
    
    func sign(_ signIn: GIDSignIn!, didSignInFor user: GIDGoogleUser!, withError error: Error!) {
        if let error = error {
            print("\(error.localizedDescription)")
            // [START_EXCLUDE silent]
            NotificationCenter.default.post(
                name: Notification.Name(rawValue: "ToggleAuthUINotification"), object: nil, userInfo: nil)
            // [END_EXCLUDE]
        } else {
            // Perform any operations on signed in user here.
//            let userId = user.userID                  // For client-side use only!
            MyAppDelegate.idToken = user.authentication.idToken // Safe to send to the server
            let fullName = user.profile.name
//            let givenName = user.profile.givenName
//            let familyName = user.profile.familyName
//            let email = user.profile.email
            // [START_EXCLUDE]
            NotificationCenter.default.post(
                name: Notification.Name(rawValue: "ToggleAuthUINotification"),
                object: nil,
                userInfo: ["statusText": "Signed in: \(fullName!)"])
            // [END_EXCLUDE]
            
            if postToServer(MyAppDelegate.idToken!) {
                print("Log-in request was a success!")
            } else {
                print("Log-in request failed!")
            }
        }
    }
    
    func postToServer(_ idToken: String) -> Bool {
        print("DOING THE THING!")
        let url = URL(string: "http://10.11.195.133:4000/login/\(idToken)/")!
        let session = URLSession.shared
        var request = URLRequest(url: url)
        request.httpMethod = "GET"
        
        print(request)
        let task = session.dataTask(with: request as URLRequest, completionHandler: { data, response, error in
            guard error == nil else {
                return
            }

            guard let data = data else {
                return
            }

            do {
                if let json = try JSONSerialization.jsonObject(with: data, options: .mutableContainers) as? [String: Any] {
                    print(json)
                }
            } catch let error {
                print(error.localizedDescription)
                return
            }
        })
        task.resume()
        print(task.response ?? (task.error ?? "No error or responce."))
        return true
    }

    func accessEndpoint(_ idToken: String) -> Bool {
        print("DOING THE THING!")
        let url = URL(string: "http://10.11.195.133:4000/login/")!
        let session = URLSession.shared
        var request = URLRequest(url: url)
        request.httpMethod = "GET"
        
        //        do {
        //            request.httpBody = try JSONSerialization.data(withJSONObject: userData, options: .prettyPrinted)
        //        } catch let error {
        //            print(error.localizedDescription)
        //            return false
        //        }
        //
        //        request.addValue("application/json", forHTTPHeaderField: "Content-Type")
        //        request.addValue("application/json", forHTTPHeaderField: "Accept")
        
        print(request)
        let task = session.dataTask(with: request as URLRequest, completionHandler: { data, response, error in
            guard error == nil else {
                return
            }
            
            guard let data = data else {
                return
            }
            
            do {
                if let json = try JSONSerialization.jsonObject(with: data, options: .mutableContainers) as? [String: Any] {
                    print(json)
                }
            } catch let error {
                print(error.localizedDescription)
                return
            }
        })
        //        do {
        //
        //
        //        } catch let error {
        //            print(error.localizedDescription)
        //            return false
        //        }
        task.resume()
//        print(task.response ?? (task.error ?? "No error or responce."))
        return true
    }
    
    var window: UIWindow?


    func application(_ application: UIApplication, didFinishLaunchingWithOptions launchOptions: [UIApplicationLaunchOptionsKey: Any]?) -> Bool {
        // Override point for customization after application launch.
        GIDSignIn.sharedInstance().clientID = "941224328342-hn8vp3mel5l8ufvbmvpj13vg093sgg2f.apps.googleusercontent.com"
        GIDSignIn.sharedInstance().delegate = self
        
        
        return true
    }
    
    func application(_ application: UIApplication,
                     open url: URL, sourceApplication: String?, annotation: Any) -> Bool {
        return GIDSignIn.sharedInstance().handle(url,
                                                 sourceApplication: sourceApplication,
                                                 annotation: annotation)
    }

    @available(iOS 9.0, *)
    func application(_ app: UIApplication, open url: URL, options: [UIApplicationOpenURLOptionsKey : Any]) -> Bool {
        return GIDSignIn.sharedInstance().handle(url,
                                                 sourceApplication: options[UIApplicationOpenURLOptionsKey.sourceApplication] as? String,
                                                 annotation: options[UIApplicationOpenURLOptionsKey.annotation])
    }
    
    func applicationWillResignActive(_ application: UIApplication) {
        // Sent when the application is about to move from active to inactive state. This can occur for certain types of temporary interruptions (such as an incoming phone call or SMS message) or when the user quits the application and it begins the transition to the background state.
        // Use this method to pause ongoing tasks, disable timers, and invalidate graphics rendering callbacks. Games should use this method to pause the game.
    }

    func applicationDidEnterBackground(_ application: UIApplication) {
        // Use this method to release shared resources, save user data, invalidate timers, and store enough application state information to restore your application to its current state in case it is terminated later.
        // If your application supports background execution, this method is called instead of applicationWillTerminate: when the user quits.
    }

    func applicationWillEnterForeground(_ application: UIApplication) {
        // Called as part of the transition from the background to the active state; here you can undo many of the changes made on entering the background.
    }

    func applicationDidBecomeActive(_ application: UIApplication) {
        // Restart any tasks that were paused (or not yet started) while the application was inactive. If the application was previously in the background, optionally refresh the user interface.
    }

    func applicationWillTerminate(_ application: UIApplication) {
        // Called when the application is about to terminate. Save data if appropriate. See also applicationDidEnterBackground:.
    }

}

