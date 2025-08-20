#include <SFML/Graphics.hpp>
#include <cstdio>
#include <cstring>
#include <iostream>
#include <string>
#include <sstream>
#include <optional>
#include <functional>
#include <memory>
#include <unistd.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <fcntl.h>

// Simple debug switch for verbose GUI/back-end I/O logging
static const bool DEBUG_LOG = true;

struct Button {
    sf::RectangleShape rect;
    sf::Text label;
    std::function<void()> onClick;
    bool contains(sf::Vector2f p) const { return rect.getGlobalBounds().contains(p); }
};

class BackendPipe {
public:
    explicit BackendPipe(std::string backendPath) : path(std::move(backendPath)) {
        // Expand $HOME prefix for an absolute path
        std::string exe = path;
        const char* home = getenv("HOME");
        if (!exe.empty() && exe.rfind("$HOME/", 0) == 0 && home) {
            exe = std::string(home) + "/" + exe.substr(6);
        }
        resolved_exe_ = exe;
        std::cerr << "Backend exe: " << resolved_exe_ << "\n";

        int inpipe[2];  // parent writes -> child stdin
        int outpipe[2]; // child stdout -> parent reads
        if (pipe(inpipe) == -1 || pipe(outpipe) == -1) {
            std::cerr << "pipe() failed\n";
            return;
        }
        pid_ = fork();
        if (pid_ < 0) {
            std::cerr << "fork() failed\n";
            close(inpipe[0]); close(inpipe[1]);
            close(outpipe[0]); close(outpipe[1]);
            pid_ = -1; return;
        }
        if (pid_ == 0) {
            // Child
            dup2(inpipe[0], STDIN_FILENO);
            dup2(outpipe[1], STDOUT_FILENO);
            close(inpipe[0]); close(inpipe[1]);
            close(outpipe[0]); close(outpipe[1]);
            execl(resolved_exe_.c_str(), resolved_exe_.c_str(), (char*)nullptr);
            _exit(127);
        }
        // Parent keeps ends
        close(inpipe[0]);
        close(outpipe[1]);
        in_fd_ = inpipe[1];
        out_fd_ = outpipe[0];
    }

    ~BackendPipe() {
        if (pid_ > 0) {
            // Try to tell backend to exit gracefully
            const char* exitMsg = "{\"op\":\"exit\"}\n";
            if (in_fd_ != -1) {
                (void)write(in_fd_, exitMsg, strlen(exitMsg));
                close(in_fd_);
                in_fd_ = -1;
            }
            if (out_fd_ != -1) {
                // Drain one line if available (non-blocking best effort)
                char buf[256];
                (void)read(out_fd_, buf, sizeof(buf));
                close(out_fd_);
                out_fd_ = -1;
            }
            int status = 0; (void)waitpid(pid_, &status, 0);
            pid_ = -1;
        }
    }

    std::string request(const std::string& jsonLine) {
        if (pid_ <= 0 || in_fd_ == -1 || out_fd_ == -1) {
            return R"({"ok":false,"error":"backend_not_running"})";
        }
        // Write payload + newline
        std::string payload = jsonLine;
        payload.push_back('\n');
        if (DEBUG_LOG) {
            std::cerr << "[GUI] >> " << jsonLine << "\n";
        }
        ssize_t w = write(in_fd_, payload.data(), payload.size());
        (void)w;
        // Read first line
        std::string out;
        char c;
        while (true) {
            ssize_t r = read(out_fd_, &c, 1);
            if (r <= 0) break;
            if (c == '\n' || c == '\r') break;
            out.push_back(c);
            if (out.size() > 1000) break;
        }
        if (out.empty()) return R"({"ok":false,"error":"no_response"})";
        if (DEBUG_LOG) {
            std::cerr << "[GUI] << " << out << "\n";
        }
        return out;
    }
private:
    std::string path; // e.g. $HOME/atm_backend or /home/user/atm_backend
    pid_t pid_ {-1};
    int in_fd_ {-1};
    int out_fd_ {-1};
    std::string resolved_exe_;
};

int main() {
    // --- SFML setup ---
    sf::RenderWindow window(sf::VideoMode(700, 420), "ATM (COBOL backend)");
    window.setFramerateLimit(60);
    const float BASE_W = 700.f, BASE_H = 420.f; // virtual canvas size
    sf::View view(sf::FloatRect(0.f, 0.f, BASE_W, BASE_H));
    window.setView(view);

    auto applyLetterbox = [&](unsigned int winW, unsigned int winH){
        float windowRatio = static_cast<float>(winW) / static_cast<float>(winH);
        float viewRatio   = BASE_W / BASE_H;
        float sizeX = 1.f, sizeY = 1.f;
        float posX  = 0.f, posY  = 0.f;
        if (windowRatio > viewRatio) {
            // window wider: add horizontal bars
            sizeX = viewRatio / windowRatio;
            posX = (1.f - sizeX) * 0.5f;
        } else if (windowRatio < viewRatio) {
            // window taller: add vertical bars
            sizeY = windowRatio / viewRatio;
            posY = (1.f - sizeY) * 0.5f;
        }
        view.setViewport(sf::FloatRect(posX, posY, sizeX, sizeY));
        window.setView(view);
    };
    applyLetterbox(window.getSize().x, window.getSize().y);

    bool fullscreen = false;
    auto recreateWindow = [&](){
        if (fullscreen) {
            auto mode = sf::VideoMode::getDesktopMode();
            window.create(mode, "ATM (COBOL backend)", sf::Style::Fullscreen);
        } else {
            window.create(sf::VideoMode(static_cast<unsigned int>(BASE_W), static_cast<unsigned int>(BASE_H)),
                         "ATM (COBOL backend)", sf::Style::Default);
        }
        window.setFramerateLimit(60);
        window.setView(view);
        applyLetterbox(window.getSize().x, window.getSize().y);
    };

    // --- Palette (closest to provided) ---
    const sf::Color COL_BG(255,255,255);        // white background
    const sf::Color COL_PRIMARY(0x6F,0xCD,0xFA); // #6FCDFA
    const sf::Color COL_ACCENT(0x7E,0xB6,0xCF);  // #7EB6CF
    const sf::Color COL_MUTED(0x80,0x99,0xA5);   // #8099A5
    const sf::Color COL_TEXT(0x37,0x48,0x50);    // #374850
    const sf::Color COL_DARK(0x1B,0x2B,0x33);    // #1B2B33
    const sf::Color COL_HOVER(0x69,0x75,0x7A);   // #69757A (for outlines)

    sf::Font font;
    if (!font.loadFromFile("assets/fonts/arial.ttf")) {
        std::cerr << "Font not found\n";
        return 1;
    }

    // --- Backend ---
    std::unique_ptr<BackendPipe> backend = std::make_unique<BackendPipe>("./atm_backend");
    auto restartBackend = [&](){
        backend.reset();
        backend = std::make_unique<BackendPipe>("./atm_backend");
    };

    // --- UI state ---
    std::string cardHashInput, pinInput, amountInput;
    std::string status = "Veuillez vous connecter pour continuer.";
    bool logged = false;
    double balanceValue = 0.0;

    // UTF-8 helper
    auto U8 = [&](const std::string& s){ return sf::String::fromUtf8(s.begin(), s.end()); };

    // Widgets
    auto makeButton = [&](float x, float y, const sf::String& text, std::function<void()> cb){
        Button b;
        b.rect.setPosition(x,y);
        b.rect.setSize({150,40}); // reduced width so all buttons fit
        b.rect.setFillColor(COL_PRIMARY);
        b.rect.setOutlineThickness(2);
        b.rect.setOutlineColor(COL_DARK);
        b.label.setFont(font);
        b.label.setCharacterSize(18);
        b.label.setString(text);
        b.label.setFillColor(sf::Color::White);
        // center label within button rect dynamically
        auto bounds = b.label.getLocalBounds();
        float halfW = b.rect.getSize().x * 0.5f;
        float halfH = b.rect.getSize().y * 0.5f;
        b.label.setPosition(x + halfW - bounds.width * 0.5f, y + halfH - bounds.height * 0.5f - bounds.top);
        b.onClick = cb;
        return b;
    };
    auto setButtonPos = [&](Button& b, float x, float y){
        b.rect.setPosition(x,y);
        // recenter label in rect
        auto bounds = b.label.getLocalBounds();
        float halfW = b.rect.getSize().x * 0.5f;
        float halfH = b.rect.getSize().y * 0.5f;
        b.label.setPosition(x + halfW - bounds.width * 0.5f, y + halfH - bounds.height * 0.5f - bounds.top);
    };

    // Helper JSON parsing (very naive)
    auto parseOk = [&](const std::string& s){ return s.find("\"ok\":true") != std::string::npos; };
    auto parseBalance = [&](const std::string& s)->std::optional<double>{
        std::size_t p = s.find("\"balance\":");
        if (p == std::string::npos) return std::nullopt;
        p += std::string("\"balance\":").size();
        std::size_t q = p;
        while (q < s.size() && (std::isdigit(static_cast<unsigned char>(s[q])) || s[q]=='.' || s[q]=='-' )) q++;
        try {
            double v = std::stod(s.substr(p, q-p));
            return v;
        } catch (...) { return std::nullopt; }
    };

    Button btnLogin   = makeButton(30, 150, U8(u8"SE CONNECTER"),   [&](){
        std::ostringstream oss;
        oss << R"({"op":"login","hash-carte":")" << cardHashInput << R"(","pin":")" << pinInput << R"("})";
        if (DEBUG_LOG) {
            std::cerr << "[GUI] login click: hash-carte=" << cardHashInput
                      << ", pin=" << pinInput << "\n";
        }
        std::string resp = backend->request(oss.str());
        if (parseOk(resp)) {
            logged = true;
            status = "Connexion réussie, bienvenue !";
            // Get initial balance
            std::string bresp = backend->request(std::string(R"({"op":"balance","hash-carte":")") + cardHashInput + R"("})");
            if (auto bv = parseBalance(bresp)) balanceValue = *bv;
            if (DEBUG_LOG) {
                std::cerr << "[GUI] balance after login: resp=" << bresp
                          << ", parsed=" << balanceValue << "\n";
            }
            // Layout switch is handled by the mouse click handler after login
        } else {
            status = "PIN incorrect. Veuillez réessayer.";
            if (DEBUG_LOG) {
                std::cerr << "[GUI] login failed. resp=" << resp << "\n";
            }
            // Keep login layout consistent (handled by layoutLogin())
        }
    });
    Button btnDeposit = makeButton(370,150, U8(u8"DÉPÔT"), [&](){
        std::ostringstream oss;
        oss << R"({"op":"deposit","hash-carte":")" << cardHashInput << R"(","amount":)" << (amountInput.empty() ? "0" : amountInput) << "}";
        std::string resp = backend->request(oss.str());
        if (auto bv = parseBalance(resp)) balanceValue = *bv;
        status = parseOk(resp) ? "Dépôt effectué." : "Erreur lors du dépôt.";
    });
    Button btnWithdraw= makeButton(540,150, U8(u8"RETRAIT"),[&](){
        std::ostringstream oss;
        oss << R"({"op":"withdraw","hash-carte":")" << cardHashInput << R"(","amount":)" << (amountInput.empty() ? "0" : amountInput) << "}";
        std::string resp = backend->request(oss.str());
        if (auto bv = parseBalance(resp)) balanceValue = *bv;
        status = parseOk(resp) ? "Retrait effectué." : "Erreur lors du retrait.";
    });

    // Quick withdraw buttons (select amount only; withdrawal occurs when clicking RETRAIT)
    Button btnW10  = makeButton(30, 260,  U8(u8"10 €"),  [&](){ amountInput = "10";  status = u8"Montant sélectionné: 10 €"; });
    Button btnW20  = makeButton(190, 260, U8(u8"20 €"),  [&](){ amountInput = "20";  status = u8"Montant sélectionné: 20 €"; });
    Button btnW50  = makeButton(350, 260, U8(u8"50 €"),  [&](){ amountInput = "50";  status = u8"Montant sélectionné: 50 €"; });
    Button btnW100 = makeButton(510, 260, U8(u8"100 €"), [&](){ amountInput = "100"; status = u8"Montant sélectionné: 100 €"; });
    Button btnLogout = makeButton(540, 20, U8(u8"DÉCONNEXION"), [&](){
        // End backend session and reset UI
        (void)backend->request(R"({"op":"exit"})");
        restartBackend();
        logged = false;
        cardHashInput.clear();
        pinInput.clear();
        amountInput.clear();
        balanceValue = 0.0;
        status = "Veuillez vous connecter pour continuer.";
    });

    // Input labels
    sf::Text lblHash(U8(u8"Hash carte:"), font, 18); lblHash.setPosition(30, 10); lblHash.setFillColor(COL_TEXT);
    sf::Text lblPIN(U8("PIN:"), font, 18); lblPIN.setPosition(30, 40); lblPIN.setFillColor(COL_TEXT);
    sf::Text lblAmount(U8(u8"Montant:"), font, 18); lblAmount.setPosition(30, 90); lblAmount.setFillColor(COL_TEXT);

    // Input fields (drawn as rectangles + text)
    sf::RectangleShape boxHash({200,30}); boxHash.setPosition(120, 5); boxHash.setFillColor(COL_BG); boxHash.setOutlineThickness(2); boxHash.setOutlineColor(COL_MUTED);
    sf::RectangleShape boxPIN({200,30}); boxPIN.setPosition(120, 35); boxPIN.setFillColor(COL_BG); boxPIN.setOutlineThickness(2); boxPIN.setOutlineColor(COL_MUTED);
    sf::RectangleShape boxAmount({200,30}); boxAmount.setPosition(120, 85); boxAmount.setFillColor(COL_BG); boxAmount.setOutlineThickness(2); boxAmount.setOutlineColor(COL_MUTED);

    sf::Text txtHash("", font, 18); txtHash.setPosition(125, 10); txtHash.setFillColor(COL_DARK);
    sf::Text txtPIN("", font, 18); txtPIN.setPosition(125, 40); txtPIN.setFillColor(COL_DARK);
    sf::Text txtAmount("", font, 18); txtAmount.setPosition(125, 90); txtAmount.setFillColor(COL_DARK);

    bool focusHash = true, focusPIN = false, focusAmount = false;

    sf::Text txtStatus("", font, 16);
    txtStatus.setPosition(30, 210);
    txtStatus.setFillColor(COL_TEXT);

    // Solde display
    sf::RectangleShape boxSolde({640, 60});
    boxSolde.setPosition(30, 330);
    boxSolde.setFillColor(sf::Color(240, 248, 255));
    boxSolde.setOutlineThickness(2);
    boxSolde.setOutlineColor(COL_ACCENT);
    sf::Text txtSolde("", font, 22);
    txtSolde.setPosition(40, 345);
    txtSolde.setFillColor(COL_DARK);

    // Layout helpers
    auto centerTextHoriz = [&](sf::Text& t, float y){
        auto b = t.getLocalBounds();
        t.setPosition((BASE_W - b.width) * 0.5f - b.left, y);
    };
    auto layoutLogin = [&](){
        // Stack centered: PIN label, PIN box, Login button, status
        // Box width 200 kept; center horizontally
        float boxW = boxPIN.getSize().x;
        float xBox = (BASE_W - boxW) * 0.5f;
        // Hash field (top)
        boxHash.setPosition(xBox, 90);
        txtHash.setPosition(xBox + 5, 95);
        centerTextHoriz(lblHash, 60);
        // PIN field (below hash)
        boxPIN.setPosition(xBox, 165);
        txtPIN.setPosition(xBox + 5, 165 + 5);
        // Label centered above PIN (leave visible gap)
        lblPIN.setString(U8("PIN:"));
        centerTextHoriz(lblPIN, 135);
        // Login button centered below (raised)
        setButtonPos(btnLogin, (BASE_W - btnLogin.rect.getSize().x) * 0.5f, 215);
        // Status centered at bottom area (a bit lower)
        centerTextHoriz(txtStatus, 340);
    };
    auto layoutLogged = [&](){
        // Amount label/box on left
        boxAmount.setPosition(120, 85);
        txtAmount.setPosition(125, 90);
        // Align label next to the input field
        lblAmount.setPosition(30, 90);
        // Main action buttons distributed across width
        float margin = 30.f;
        float areaW = BASE_W - margin*2;
        float btnW = btnDeposit.rect.getSize().x;
        float y1 = 150.f;
        // Use the same spacing as quick withdraws (4 buttons -> 3 gaps)
        float qspacing = (areaW - 4*btnW) / 3.f;
        // Align two buttons (deposit, withdraw) to the right with qspacing between them
        float blockW = 2*btnW + qspacing;
        float x1 = margin + areaW - blockW; // left of the right-aligned pair
        setButtonPos(btnDeposit, x1, y1);
        setButtonPos(btnWithdraw, x1 + btnW + qspacing, y1);
        // Quick withdraw row evenly spaced (4 buttons -> 3 gaps)
        float qbtnW = btnW;
        float qspacing2 = (areaW - 4*qbtnW) / 3.f;
        float y2 = 260.f;
        setButtonPos(btnW10,  margin, y2);
        setButtonPos(btnW20,  margin + (qbtnW + qspacing2)*1, y2);
        setButtonPos(btnW50,  margin + (qbtnW + qspacing2)*2, y2);
        setButtonPos(btnW100, margin + (qbtnW + qspacing2)*3, y2);
        // Logout top-right
        setButtonPos(btnLogout, BASE_W - margin - btnLogout.rect.getSize().x, 20);
        // Solde box spans width with margins
        boxSolde.setPosition(margin, 330);
        boxSolde.setSize({areaW, 60});
        txtSolde.setPosition(margin + 10, 345);
        // Status bottom-left
        txtStatus.setPosition(margin, 310);
    };

    // Initial layout (login view)
    layoutLogin();

    while (window.isOpen()) {
        sf::Event e;
        while (window.pollEvent(e)) {
            if (e.type == sf::Event::Closed) window.close();
            if (e.type == sf::Event::Resized) {
                applyLetterbox(e.size.width, e.size.height);
                if (logged) layoutLogged(); else layoutLogin();
            }
            if (e.type == sf::Event::KeyPressed && e.key.code == sf::Keyboard::F11) {
                fullscreen = !fullscreen;
                recreateWindow();
                continue; // avoid processing same event further
            }
            if (e.type == sf::Event::MouseButtonPressed && e.mouseButton.button == sf::Mouse::Left) {
                sf::Vector2f m(e.mouseButton.x, e.mouseButton.y);
                // Map to view coordinates in case of letterboxing
                sf::Vector2f mWorld = window.mapPixelToCoords(sf::Vector2i(e.mouseButton.x, e.mouseButton.y), view);
                if (!logged) {
                    focusHash = boxHash.getGlobalBounds().contains(mWorld);
                    focusPIN = boxPIN.getGlobalBounds().contains(mWorld);
                    focusAmount = false;
                } else {
                    focusPIN = false;
                    focusHash = false;
                    focusAmount = boxAmount.getGlobalBounds().contains(mWorld);
                }

                if (!logged && btnLogin.contains(mWorld)) {
                    btnLogin.onClick();
                    if (logged) {
                        layoutLogged();
                    } else {
                        layoutLogin();
                    }
                }
                if (logged) {
                    if (btnDeposit.contains(mWorld)) btnDeposit.onClick();
                    if (btnWithdraw.contains(mWorld)) btnWithdraw.onClick();
                    if (btnW10.contains(mWorld)) btnW10.onClick();
                    if (btnW20.contains(mWorld)) btnW20.onClick();
                    if (btnW50.contains(mWorld)) btnW50.onClick();
                    if (btnW100.contains(mWorld)) btnW100.onClick();
                    if (btnLogout.contains(mWorld)) { btnLogout.onClick(); layoutLogin(); }
                }
            }
            if (e.type == sf::Event::KeyPressed && e.key.code == sf::Keyboard::Tab) {
                // Toggle focus on TAB
                if (!logged) {
                    // cycle between hash and PIN
                    if (focusHash) { focusHash = false; focusPIN = true; }
                    else if (focusPIN) { focusPIN = false; focusHash = true; }
                    else { focusHash = true; focusPIN = false; }
                } else {
                    // only amount when logged
                    focusAmount = !focusAmount;
                }
            }
            if (e.type == sf::Event::TextEntered) {
                if (e.text.unicode == 8) { // backspace
                    if (focusHash && !cardHashInput.empty()) cardHashInput.pop_back();
                    if (focusPIN && !pinInput.empty()) pinInput.pop_back();
                    if (focusAmount && !amountInput.empty()) amountInput.pop_back();
                } else if (e.text.unicode >= 32 && e.text.unicode < 127) {
                    char c = static_cast<char>(e.text.unicode);
                    if (focusHash) {
                        if ((std::isalnum(static_cast<unsigned char>(c)) || c=='-' || c=='_') && cardHashInput.size() < 32) cardHashInput.push_back(c);
                    } else if (focusPIN) {
                        if (std::isdigit(static_cast<unsigned char>(c)) && pinInput.size() < 8) pinInput.push_back(c);
                    } else if (focusAmount) {
                        if ((std::isdigit(static_cast<unsigned char>(c)) || c=='.' || c=='-') && amountInput.size() < 16) amountInput.push_back(c);
                    }
                }
            }
        }

        // Hover styles and focus styles
        sf::Vector2i mp = sf::Mouse::getPosition(window);
        sf::Vector2f mv = window.mapPixelToCoords(mp, view);
        auto hoverOutline = COL_ACCENT; // hover accent
        // Buttons hover visual
        auto styleButton = [&](Button& b){
            bool hov = b.contains(mv);
            b.rect.setOutlineThickness(hov ? 3.f : 2.f);
            b.rect.setOutlineColor(hov ? hoverOutline : COL_DARK);
            b.rect.setFillColor(hov ? COL_ACCENT : COL_PRIMARY);
            b.label.setFillColor(sf::Color::White);
        };
        if (!logged) styleButton(btnLogin);
        if (logged) {
            styleButton(btnDeposit);
            styleButton(btnWithdraw);
            styleButton(btnW10);
            styleButton(btnW20);
            styleButton(btnW50);
            styleButton(btnW100);
            styleButton(btnLogout);
        }

        // Inputs hover & focus
        bool hoverHash = boxHash.getGlobalBounds().contains(mv);
        bool hoverPIN = boxPIN.getGlobalBounds().contains(mv);
        bool hoverAmount = boxAmount.getGlobalBounds().contains(mv);
        auto focusedOutline = COL_PRIMARY;
        // Hash box style (login only)
        boxHash.setOutlineThickness((focusHash || hoverHash) ? 3.f : 2.f);
        boxHash.setOutlineColor(focusHash ? focusedOutline : (hoverHash ? hoverOutline : COL_MUTED));
        boxHash.setFillColor(focusHash ? COL_ACCENT : COL_BG);
        // PIN box style
        boxPIN.setOutlineThickness((focusPIN || hoverPIN) ? 3.f : 2.f);
        boxPIN.setOutlineColor(focusPIN ? focusedOutline : (hoverPIN ? hoverOutline : COL_MUTED));
        boxPIN.setFillColor(focusPIN ? COL_ACCENT : COL_BG);
        // Amount box style
        boxAmount.setOutlineThickness((focusAmount || hoverAmount) ? 3.f : 2.f);
        boxAmount.setOutlineColor(focusAmount ? focusedOutline : (hoverAmount ? hoverOutline : COL_MUTED));
        boxAmount.setFillColor(focusAmount ? COL_ACCENT : COL_BG);

        txtHash.setString(cardHashInput);
        txtPIN.setString(std::string(pinInput.size(), '*')); // masque PIN
        txtAmount.setString(amountInput);
        {
            // status is UTF-8
            txtStatus.setString(U8(status));
            // Re-center status on login page (text width can change)
            if (!logged) centerTextHoriz(txtStatus, 270);
        }
        {
            std::ostringstream ss;
            ss.setf(std::ios::fixed); ss.precision(2);
            ss << "Solde: " << balanceValue << " " << (const char*)u8"€";
            txtSolde.setString(U8(ss.str()));
        }

        // Draw
        window.clear(COL_BG);
        if (!logged) {
            // Login page: center-stacked
            window.draw(lblHash); window.draw(boxHash); window.draw(txtHash);
            window.draw(lblPIN);  window.draw(boxPIN);  window.draw(txtPIN);
            window.draw(btnLogin.rect);   window.draw(btnLogin.label);
        } else {
            // Amount controls
            window.draw(lblAmount); window.draw(boxAmount); window.draw(txtAmount);
            // Main action buttons
            window.draw(btnDeposit.rect); window.draw(btnDeposit.label);
            window.draw(btnWithdraw.rect);window.draw(btnWithdraw.label);
            // Quick withdraws
            window.draw(btnW10.rect);  window.draw(btnW10.label);
            window.draw(btnW20.rect);  window.draw(btnW20.label);
            window.draw(btnW50.rect);  window.draw(btnW50.label);
            window.draw(btnW100.rect); window.draw(btnW100.label);
            // Logout
            window.draw(btnLogout.rect); window.draw(btnLogout.label);
            // Solde box
            window.draw(boxSolde); window.draw(txtSolde);
        }

        window.draw(txtStatus);
        window.display();
    }

    // Optional: notify backend to exit (not strictly required)
    backend->request(R"({"op":"exit"})");
    return 0;
}
